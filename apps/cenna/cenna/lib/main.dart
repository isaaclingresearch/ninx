import 'dart:convert';

import 'package:flutter/material.dart';
import 'dart:ffi';
import 'dart:io';

import 'package:intl/intl.dart'; // For date formatting
import 'package:country_picker/country_picker.dart';
import 'package:intl_phone_field/intl_phone_field.dart';
import 'package:intl_phone_field/phone_number.dart';
import 'package:path/path.dart';
import 'package:sqlite3/open.dart';
import 'package:sqlite3/sqlite3.dart' hide Row;
import 'package:path_provider/path_provider.dart';
import 'package:uuid/uuid.dart';
import 'package:http/http.dart' as http;

var uuid = Uuid();
var dev = false;
Future<String> getDbPath() async {
  open.overrideFor(OperatingSystem.linux, _openOnLinux);
  final directory = Platform.isIOS
      ? await getLibraryDirectory()
      : await getApplicationDocumentsDirectory();
  return directory.path;
}

var dbName = dev ? '/cenna${uuid.v4()}.db' : '/cenna.db';
void main() async {
  String path = '${await getDbPath()}$dbName';
  final db = sqlite3.open(path);
  // create tables
  db.execute('''
    CREATE TABLE IF NOT EXISTS system_variables (
      variable text primary key,
      value text,
      set_date text default CURRENT_TIMESTAMP);

    CREATE TABLE IF NOT EXISTS user_ids (
      user_id text primary key,
      created_at default CURRENT_TIMESTAMP);

    CREATE TABLE IF NOT EXISTS registered_users (
      user_id text primary key,
      created_at default CURRENT_TIMESTAMP);

    CREATE TABLE IF NOT EXISTS registered_demographics (
      demographic TEXT,
      value TEXT,
      user_id TEXT,
      set_date DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (demographic, user_id)
      FOREIGN KEY (user_id) REFERENCES user_ids(user_id));

    CREATE TABLE IF NOT EXISTS demographics (
      demographic TEXT,
      value TEXT,
      user_id TEXT,
      set_date DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (demographic, user_id)
      FOREIGN KEY (user_id) REFERENCES user_ids(user_id));
    ''');
  runApp(Cenna(
    dbPath: path,
  ));
  db.dispose();
}

DynamicLibrary _openOnLinux() {
  final scriptDir = File(Platform.script.toFilePath()).parent;
  final libraryNextToScript = File(join(scriptDir.path, 'libsqlite3.so.0'));
  return DynamicLibrary.open(libraryNextToScript.path);
}

class Cenna extends StatelessWidget {
  final String dbPath;
  Cenna({super.key, required this.dbPath});

  // check for active user_id, if its in the registered users table, it means it has been saved go to
  // Home, otherwise, for now go to the demographics with the incomplete user_id
  String _checkActiveUserType() {
    final db = sqlite3.open(dbPath);
    final query =
        db.prepare('''select value from system_variables where variable=?''');
    final ResultSet entry = query.select(['current_user_id']);
    query.dispose();
    String finalResult = '';
    if (entry.isEmpty) {
      finalResult = 'start-registration';
    } else {
      final value = entry[0]['value'];
      if (value != null) {
        final query1 = db
            .prepare('''select * from registered_users where user_id=?''');
        final ResultSet results = query1.select([value]);
        if (results.isEmpty) {
          finalResult = 'continue-registration';
        } else {
          finalResult = 'go-home';
        }
        query1.dispose();
      }
    }
    query.dispose();
    db.dispose();
    return finalResult;
  }

  Widget? home;
  @override
  Widget build(BuildContext context) {
    var action = _checkActiveUserType();
    switch (action) {
      case 'start-registration':
        home = DemographicsForm();
        break;
      case 'continue-registration':
        home = DemographicsForm();
        break;
      case 'go-home':
        home = Home();
    }

    return MaterialApp(
      title: 'Cenna',
      home: home,
    );
  }
}

String? validateEmail(String? value) {
  if (value == null || value.isEmpty) {
    return 'Please enter an email address.';
  }

  // Regular expression for email validation
  final emailRegex = RegExp(
      r"^[a-zA-Z0-9.a-zA-Z0-9.!#$%&'*+-/=?^_`{|}~]+@[a-zA-Z0-9]+\.[a-zA-Z]+");

  if (!emailRegex.hasMatch(value)) {
    return 'Please enter a valid email address.';
  }

  return null; // Return null if the email is valid
}

class DemographicsForm extends StatefulWidget {
  const DemographicsForm({super.key});
  @override
  State<DemographicsForm> createState() => _DemographicsFormState();
}

class _DemographicsFormState extends State<DemographicsForm> {
  int currentIndex = 0;
  final PageController _pageController = PageController(initialPage: 0);
  final int _numPages = 3; // Number of pages
  final List<GlobalKey<FormState>> _formKeys =
      List.generate(4, (_) => GlobalKey<FormState>());

  late Directory directory;
  late String dbPath;
  late String userId;

  @override
  void initState() {
    super.initState();
    _initializeDirectory().then((_) {
      _initialiseValues();
    });
  }

  Future<void> _initializeDirectory() async {
    final dir = await (Platform.isIOS
        ? getLibraryDirectory()
        : getApplicationDocumentsDirectory());
    setState(() {
      directory = dir; // Assign the directory after fetching it
      dbPath = '${dir.path}$dbName';
      userId = _getActiveUserId();
    });
  }

// this will return a saved variable to use it to repopulate the forms
  String _getActiveUserId() {
    final db = sqlite3.open(dbPath);
    final query =
        db.prepare('''select value from system_variables where variable=?''');
    final ResultSet entry = query.select(['current_user_id']);
    query.dispose();
    db.dispose();
    if (entry.isEmpty) {
      return uuid.v4();
    } else {
      final value = entry[0]['value'];
      return (value == null) ? uuid.v4() : value;
    }
  }

// this will return a saved variable to use it to repopulate the forms
  String _getSavedValue(String val) {
    final db = sqlite3.open(dbPath);
    final query = db.prepare(
        '''select value from demographics where demographic=? AND user_id = ?''');
    final ResultSet entry = query.select([val, userId]);
    var finalVal = '';
    if (entry.isNotEmpty) {
      finalVal = entry[0]['value'];
    }
    query.dispose();
    db.dispose();
    return finalVal;
  }

  // intialisevalues
  void _initialiseValues() {
    final fullName = _getSavedValue('full-name');
    final sex = _getSavedValue('sex'); // This will hold "Male" or "Female"
    final gender = _getSavedValue('gender');
    final race = _getSavedValue('race');
    final levelOfEducation = _getSavedValue('level-of-education');
    final dateOfBirth = _getSavedValue('date-of-birth');
    final countryOfOrigin = _getSavedValue('country-of-origin');
    final countryOfResidence = _getSavedValue('country-of-residence');
    final cityOfResidence = _getSavedValue('city-of-residence');
    final occupation = _getSavedValue('occupation');
    final telephoneNumber = _getSavedValue('telephone-number');
    final email = _getSavedValue('email');
    final nextOfKin = _getSavedValue('next-of-kin-name');
    final nextOfKinPhoneNumber = _getSavedValue('next-of-kin-telephone-number');
    final nextOfKinRelationship = _getSavedValue('next-of-kin-relationship');
    final nextOfKinEmail = _getSavedValue('next-of-kin-email');
    final maritalStatus = _getSavedValue('marital-status');
    setState(() {
      if (fullName != '') {
        _fullNameController.text = fullName;
      }
      if (sex != '') {
        _selectedSex = sex;
      }
      if (gender != '') {
        _selectedGender = gender;
      }
      if (race != '') {
        _selectedRace = race;
      }
      if (maritalStatus != '') {
        _selectedMaritalStatus = maritalStatus;
      }
      if (levelOfEducation != '') {
        _selectedEducation = levelOfEducation;
      }
      if (dateOfBirth != '') {
        _selectedDate = DateFormat('yyyy-MM-dd').parse(dateOfBirth);
      }
      if (occupation != '') {
        _occupationController.text = occupation;
      }
      if (cityOfResidence != '') {
        _cityController.text = cityOfResidence;
      }
      if (countryOfResidence != '') {
        _selectedCountryOfResidence =
            CountryParser.parseCountryName(countryOfResidence);
        _countryOfResidenceController.text = countryOfResidence;
      }
      if (countryOfOrigin != '') {
        _selectedCountry = CountryParser.parseCountryName(countryOfOrigin);
        _countryController.text = countryOfOrigin;
      }
      if (telephoneNumber != '') {
        _phoneNumber =
            PhoneNumber.fromCompleteNumber(completeNumber: telephoneNumber);
      }
      if (email != '') {
        _emailController.text = email;
      }
      if (nextOfKin != '') {
        _nextOfKinNameController.text = nextOfKin;
      }
      if (nextOfKinPhoneNumber != '') {
        _nextOfKinPhoneNumber = PhoneNumber.fromCompleteNumber(
            completeNumber: nextOfKinPhoneNumber);
      }
      if (nextOfKinRelationship != '') {
        _nextOfKinRelationshipController.text = nextOfKinRelationship;
      }
      if (nextOfKinEmail != '') {
        _nextOfKinEmailController.text = nextOfKinEmail;
      }
    });
  }

  // this function will check if data has been saved and then it will skip to the page without data.
  void _skipToPage() {
    final db = sqlite3.open(dbPath);
    final query = db.prepare(
        '''select value from demographics where demographic=? and user_id=?''');
    final ResultSet fullName = query.select(['full-name', userId]);
    final ResultSet occupation = query.select(['occupation', userId]);
    query.dispose();
    db.dispose();
    if (fullName == []) {
      return;
    }
    if (occupation != []) {
      setState(() {
        currentIndex = 2;
      });
    }
  }

  void _saveToDb(int page) async {
    print('page: $page\n\n');
    final directory = Platform.isIOS
        ? await getLibraryDirectory()
        : await getApplicationDocumentsDirectory();
    String path = '${directory.path}$dbName';
    final db = sqlite3.open(path);
    // save the user to allow progress to continue
    db.prepare(
        '''insert or replace into system_variables (variable, value) values (?, ?)''').execute([
      'current_user_id',
      userId
    ]);
    final query = db.prepare(
        '''insert or replace into demographics (demographic, value, user_id) values (?, ?, ?) ''');
    switch (page) {
      case 0:
        query
          ..execute(['full-name', _fullNameController.text, userId])
          ..execute(['sex', _selectedSex, userId])
          ..execute([
            'date-of-birth',
            DateFormat('yyyy-MM-dd').format(_selectedDate!),
            userId
          ])
          ..execute(['marital-status', _selectedMaritalStatus, userId])
          ..execute(['level-of-education', _selectedEducation, userId])
          ..execute(['race', _selectedRace, userId])
          ..execute(['gender', _selectedGender, userId]);
        break;
      case 1:
        query
          ..execute(['country-of-origin', _selectedCountry?.name, userId])
          ..execute([
            'country-of-residence',
            _selectedCountryOfResidence?.name,
            userId
          ])
          ..execute(['city-of-residence', _cityController.text, userId])
          ..execute(['occupation', _occupationController.text, userId]);
        break;
      case 2:
        print('2 called\n');
        query
          ..execute(['email', _emailController.text, userId])
          ..execute(['telephone-number', _phoneNumber?.completeNumber, userId])
          ..execute(['next-of-kin-name', _nextOfKinNameController.text, userId])
          ..execute(
              ['next-of-kin-email', _nextOfKinEmailController.text, userId])
          ..execute([
            'next-of-kin-relationship',
            _nextOfKinRelationshipController.text,
            userId
          ])
          ..execute([
            'next-of-kin-telephone-number',
            _nextOfKinPhoneNumber?.completeNumber,
            userId
          ]);
    }
    query.dispose();
    db.dispose();
  }

  void _saveRegisteredUserToDb(String id) async {
    final directory = Platform.isIOS
        ? await getLibraryDirectory()
        : await getApplicationDocumentsDirectory();
    String path = '${directory.path}$dbName';
    final db = sqlite3.open(path);
    // save the user to allow progress to continue
    db.prepare(
        '''insert or replace into system_variables (variable, value) values (?, ?)''').execute([
      'current_user_id',
      id
    ]);
    db.prepare(
        '''insert or replace into registered_users (user_id) values (?)''').execute([
      id
    ]);
    final query = db.prepare(
        '''insert or replace into registered_demographics (demographic, value, user_id) values (?, ?, ?) ''');
    query
      ..execute(['full-name', _fullNameController.text, id])
      ..execute(['sex', _selectedSex, id])
      ..execute([
        'date-of-birth',
        DateFormat('yyyy-MM-dd').format(_selectedDate!),
        id
      ])
      ..execute(['marital-status', _selectedMaritalStatus, id])
      ..execute(['level-of-education', _selectedEducation, id])
      ..execute(['race', _selectedRace, id])
      ..execute(['gender', _selectedGender, id])
      ..execute(['country-of-origin', _selectedCountry?.name, id])
      ..execute(['country-of-residence', _selectedCountryOfResidence?.name, id])
      ..execute(['city-of-residence', _cityController.text, id])
      ..execute(['occupation', _occupationController.text, id])
      ..execute(['email', _emailController.text, id])
      ..execute(['telephone-number', _phoneNumber?.completeNumber, id])
      ..execute(['next-of-kin-name', _nextOfKinNameController.text, id])
      ..execute(['next-of-kin-email', _nextOfKinEmailController.text, id])
      ..execute([
        'next-of-kin-relationship',
        _nextOfKinRelationshipController.text,
        id
      ])
      ..execute([
        'next-of-kin-telephone-number',
        _nextOfKinPhoneNumber?.completeNumber,
        id
      ]);
    query.dispose();
    db.dispose();
  }

  void _saveProfileToServer() async {
    var client = http.Client();
    try {
      var response = await client
          .post(Uri.https('cenna:8443', '/save-user-profile'), body: {
        'profile-json': jsonEncode({
          'email': _emailController.text,
          'sex-at-birth': _selectedSex,
          'race': _selectedRace,
          'marital-status': _selectedMaritalStatus,
          'occupation': _occupationController.text,
          'full-name': _fullNameController.text,
          'telephone-number': _phoneNumber?.completeNumber,
          'date-of-birth': DateFormat('yyyy-MM-dd').format(_selectedDate!),
          'gender': _selectedGender,
          'level-of-education': _selectedEducation,
          'country-of-origin': _selectedCountry?.name,
          'country-of-residence': _selectedCountryOfResidence?.name,
          'next-of-kin-name': _nextOfKinNameController.text,
          'next-of-kin-email': _nextOfKinEmailController.text,
          'next-of-kin-relationship': _nextOfKinRelationshipController.text,
          'next-of-kin-telephone-number': _nextOfKinPhoneNumber?.completeNumber
        })
      });
      print(response.body);
      var id = jsonDecode(response.body)['user-id'];
      // when the id is returned, save it and the name to registered-users table.
      // then save the data to the registered-profiles table
      // then redirect to home.
      _saveRegisteredUserToDb(id);
    } finally {
      client.close();
    }
  }

  // Controllers for Page 1
  final TextEditingController _fullNameController = TextEditingController();
  String? _selectedSex; // This will hold "Male" or "Female"
  String? _selectedGender;
  String? _selectedRace;
  String? _selectedEducation;
  String? _selectedMaritalStatus;
  DateTime? _selectedDate; // This will hold the selected date

  // Controllers for Page 2
  final TextEditingController _countryController = TextEditingController();
  Country? _selectedCountry;
  final TextEditingController _countryOfResidenceController =
      TextEditingController();
  Country? _selectedCountryOfResidence;
  final TextEditingController _cityController = TextEditingController();
  final TextEditingController _occupationController = TextEditingController();

  // Controllers for Page 3
  PhoneNumber? _phoneNumber;
  PhoneNumber? _nextOfKinPhoneNumber;
  final TextEditingController _emailController = TextEditingController();
  final TextEditingController _nextOfKinNameController =
      TextEditingController();
  final TextEditingController _nextOfKinEmailController =
      TextEditingController();
  final TextEditingController _nextOfKinRelationshipController =
      TextEditingController();

  // Controllers for Page 4
  final TextEditingController _page4Controller1 = TextEditingController();
  final TextEditingController _page4Controller2 = TextEditingController();

  Future<void> _selectDate(BuildContext context) async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate:
          _selectedDate ?? DateTime.now(), // Show today if no date selected
      firstDate: DateTime(1900), // Adjust range as needed
      lastDate: DateTime.now(),
    );

    if (pickedDate != null && pickedDate != _selectedDate) {
      setState(() {
        _selectedDate = pickedDate;
      });
    }
  }

  void _goToPreviousPage() {
    _pageController.previousPage(
      duration: const Duration(milliseconds: 300),
      curve: Curves.easeInOut,
    );
  }

  void _goToNextPage() {
    _pageController.nextPage(
      duration: const Duration(milliseconds: 300),
      curve: Curves.easeInOut,
    );
  }

  void _navigateToForm1(BuildContext context) {
    bool demographicsValid = true;
    for (final key in _formKeys) {
      if (key.currentState != null && !key.currentState!.validate()) {
        demographicsValid = false;
      }
    }

    if (demographicsValid) {
      Navigator.push(
          context,
          MaterialPageRoute(
            builder: (context) => Form1(
              demographicsData: {
                'fullname': _fullNameController.text,
                'sex': _selectedSex,
                'gender': _selectedGender,
                'education': _selectedEducation,
                'race': _selectedRace,
                'date-of-birth': _selectedDate != null
                    ? DateFormat('yyyy-MM-dd').format(_selectedDate!)
                    : '',
                'country-of-birth': _selectedCountry?.displayName,
                'country-of-residence':
                    _selectedCountryOfResidence?.displayName,
                'city-of-residence': _cityController.text,
                'occupation': _occupationController.text,
                'phone-number': _phoneNumber,
                'email': _emailController.text,
                'next-of-kin-name': _nextOfKinNameController.text,
                'next-of-kin-relationship':
                    _nextOfKinRelationshipController.text,
                'next-of-kin-email': _nextOfKinEmailController.text,
                'next-of-kin-phone-number': _nextOfKinPhoneNumber,
              },
            ),
          ));
    } else {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
            content: Text('Please fill out all fields on this form')),
      );
    }
  }

  void _showWhyThisDialog(int pageIndex) {
    String title = '';
    String content = '';
    switch (pageIndex) {
      case 0:
        title = 'Why Identification Data?';
        content =
            'We need to know who you are and how to address you. Some diseases are sex or age or race specific and/or related.';
        break;
      case 1:
        title = 'Why Location and residence data?';
        content =
            'Disease is affected by where you were born, where you live and the work you do.';
        break;
      case 2:
        title = 'Why contact data?';
        content =
            'We need to be able to reach to pass information in one form or another.';
        break;
      case 3:
        title = 'Why Page 4 Data?';
        content =
            'Page 4 collects final details necessary for processing your request and ensuring accuracy.';
        break;
    }
    showDialog(
      context: this.context,
      builder: (BuildContext context) {
        return AlertDialog(
          title: Text(title),
          content: Text(content),
          actions: <Widget>[
            TextButton(
              child: const Text('Close'),
              onPressed: () {
                Navigator.of(context).pop();
              },
            ),
          ],
        );
      },
    );
  }

  @override
  Widget build(BuildContext context) {
    _initializeDirectory();
    // _skipToPage();

    return Scaffold(
      appBar: AppBar(title: const Text('Demographics')),
      body: Column(
        children: [
          LinearProgressIndicator(
            color: const Color(0xFF1e90ff),
            value: currentIndex / (_numPages - 1),
            borderRadius: BorderRadius.circular(10),
            minHeight: 10.0,
          ),
          Expanded(
            child: PageView(
              controller: _pageController,
              physics: const NeverScrollableScrollPhysics(),
              onPageChanged: (i) {
                print('top\n');
                _saveToDb(currentIndex);
                print('current index: $currentIndex\n');
                setState(() {
                  currentIndex = i;
                });
              },
              children: [
                // Page 1
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Form(
                    key: _formKeys[0],
                    child: Column(
                      children: [
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            const Text('Identification'),
                            TextButton(
                              onPressed: () => _showWhyThisDialog(0),
                              child: const Text('Why this?'),
                            ),
                          ],
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _fullNameController,
                            decoration:
                                const InputDecoration(labelText: 'Full name'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter your full name.';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: DropdownButtonFormField<String>(
                            decoration: const InputDecoration(
                              labelText: 'Sex assigned at birth',
                            ),
                            value: _selectedSex, // Currently selected value
                            onChanged: (String? newValue) {
                              setState(() {
                                _selectedSex = newValue;
                              });
                            },
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please select sex assigned at birth';
                              }
                              return null;
                            },
                            items: const <DropdownMenuItem<String>>[
                              DropdownMenuItem<String>(
                                value: 'Male',
                                child: Text('Male'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Female',
                                child: Text('Female'),
                              ),
                            ],
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: DropdownButtonFormField<String>(
                            decoration: const InputDecoration(
                              labelText: 'Gender',
                            ),
                            value: _selectedGender, // Currently selected value
                            onChanged: (String? newValue) {
                              setState(() {
                                _selectedGender = newValue;
                              });
                            },
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please select sex assigned at birth';
                              }
                              return null;
                            },
                            items: const <DropdownMenuItem<String>>[
                              DropdownMenuItem<String>(
                                value: 'Male',
                                child: Text('Male'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Female',
                                child: Text('Female'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Other',
                                child: Text('Other'),
                              ),
                            ],
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            readOnly: true, // Prevent manual editing
                            onTap: () =>
                                _selectDate(context), // Open date picker on tap
                            controller: TextEditingController(
                              text: _selectedDate != null
                                  ? DateFormat('yyyy-MM-dd')
                                      .format(_selectedDate!)
                                  : '', // Format and display date
                            ),
                            decoration: const InputDecoration(
                              labelText: 'Date of Birth',
                              suffixIcon: Icon(
                                  Icons.calendar_today), // Add a calendar icon
                            ),
                            validator: (value) {
                              if (_selectedDate == null) {
                                return 'Please select a date';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: DropdownButtonFormField<String>(
                            decoration: const InputDecoration(
                              labelText: 'Race/Ethnicity',
                            ),
                            value: _selectedRace, // Currently selected value
                            onChanged: (String? newValue) {
                              setState(() {
                                _selectedRace = newValue;
                              });
                            },
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please select a race';
                              }
                              return null;
                            },
                            items: const <DropdownMenuItem<String>>[
                              DropdownMenuItem<String>(
                                value: 'African American/Black',
                                child: Text('African American/Black'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Asian',
                                child: Text('Asian'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Hispanic/Latino',
                                child: Text('Hispanic/Latino'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Indiginous American',
                                child: Text('Indiginous American'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'White',
                                child: Text('White'),
                              ),
                            ],
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: DropdownButtonFormField<String>(
                            decoration: const InputDecoration(
                              labelText: 'Highest Education',
                            ),
                            value:
                                _selectedEducation, // Currently selected value
                            onChanged: (String? newValue) {
                              setState(() {
                                _selectedEducation = newValue;
                              });
                            },
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please select your highest education level';
                              }
                              return null;
                            },
                            items: const <DropdownMenuItem<String>>[
                              DropdownMenuItem<String>(
                                value: 'Less than high school',
                                child: Text('Less than high school'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'High school',
                                child: Text('High school'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Undergraduate degree',
                                child: Text('Undergraduate degree'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Masters degree',
                                child: Text('Masters degree'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'PhD',
                                child: Text('PhD'),
                              ),
                            ],
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: DropdownButtonFormField<String>(
                            decoration: const InputDecoration(
                              labelText: 'Marital Status',
                            ),
                            value:
                                _selectedMaritalStatus, // Currently selected value
                            onChanged: (String? newValue) {
                              setState(() {
                                _selectedMaritalStatus = newValue;
                              });
                            },
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please select current marital status';
                              }
                              return null;
                            },
                            items: const <DropdownMenuItem<String>>[
                              DropdownMenuItem<String>(
                                value: 'Single',
                                child: Text('Single'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Married',
                                child: Text('Married'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Cohabiting',
                                child: Text('Cohabiting'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Other',
                                child: Text('Other'),
                              ),
                            ],
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
                // Page 2
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Form(
                    key: _formKeys[1],
                    child: Column(
                      children: [
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            const Text('Residence and Work'),
                            TextButton(
                              onPressed: () => _showWhyThisDialog(1),
                              child: const Text('Why this?'),
                            ),
                          ],
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: Column(
                            children: [
                              InkWell(
                                // Use InkWell for visual feedback on tap
                                onTap: () {
                                  showCountryPicker(
                                    context: context,
                                    showPhoneCode: false,
                                    onSelect: (Country country) {
                                      setState(() {
                                        _selectedCountry = country;
                                        _countryController.text = country
                                            .name; // Display country name
                                      });
                                    },
                                  );
                                },
                                child: IgnorePointer(
                                  // Prevents direct text input
                                  child: TextFormField(
                                    controller: _countryController,
                                    decoration: const InputDecoration(
                                        labelText:
                                            'Select Country of Birth'), // Changed label
                                    validator: (value) {
                                      if (_selectedCountry == null) {
                                        return 'Please select your country of birth';
                                      }
                                      return null;
                                    },
                                  ),
                                ),
                              ),
                            ],
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: Column(
                            children: [
                              InkWell(
                                // Use InkWell for visual feedback on tap
                                onTap: () {
                                  showCountryPicker(
                                    context: context,
                                    showPhoneCode: false,
                                    onSelect: (Country country) {
                                      setState(() {
                                        _selectedCountryOfResidence = country;
                                        _countryOfResidenceController.text =
                                            country
                                                .name; // Display country name
                                      });
                                    },
                                  );
                                },
                                child: IgnorePointer(
                                  // Prevents direct text input
                                  child: TextFormField(
                                    controller: _countryOfResidenceController,
                                    decoration: const InputDecoration(
                                        labelText:
                                            'Select current country of residence'), // Changed label
                                    validator: (value) {
                                      if (_selectedCountryOfResidence == null) {
                                        return 'Please select your current country of residence';
                                      }
                                      return null;
                                    },
                                  ),
                                ),
                              ),
                            ],
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _cityController,
                            decoration: const InputDecoration(
                                labelText: 'City of residence'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter your current city, state, or district of residence.';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _occupationController,
                            decoration:
                                const InputDecoration(labelText: 'Occupation'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter your current occupation.';
                              }
                              return null;
                            },
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
                // Page 3
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Form(
                    key: _formKeys[2],
                    child: Column(
                      children: [
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            const Text('Contacts'),
                            TextButton(
                              onPressed: () => _showWhyThisDialog(2),
                              child: const Text('Why this?'),
                            ),
                          ],
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: IntlPhoneField(
                            decoration: const InputDecoration(
                              labelText: 'Phone Number',
                              border: OutlineInputBorder(),
                            ),
                            initialValue: _phoneNumber?.number ?? '',
                            initialCountryCode: _phoneNumber?.countryISOCode ??
                                'UG', // Optional: Set an initial country code
                            onChanged: (phone) {
                              setState(() {
                                _phoneNumber = phone;
                              });
                            },
                            onCountryChanged: (country) {},
                            validator: (value) {
                              if (_phoneNumber == null ||
                                  _phoneNumber!.number.isEmpty) {
                                return 'Please enter a phone number';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _emailController,
                            decoration:
                                const InputDecoration(labelText: 'Email'),
                            validator: (value) {
                              return validateEmail(value);
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _nextOfKinNameController,
                            decoration: const InputDecoration(
                                labelText: 'Next of Kin Name'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter your next of kin';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _nextOfKinRelationshipController,
                            decoration: const InputDecoration(
                                labelText: 'Relationship with next of kin'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter your relationship with the next of kin';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: IntlPhoneField(
                            decoration: const InputDecoration(
                              labelText: 'Next of Kin\'s Phone Number',
                              border: OutlineInputBorder(),
                            ),
                            initialValue: _nextOfKinPhoneNumber?.number ?? '',
                            initialCountryCode: _nextOfKinPhoneNumber
                                    ?.countryISOCode ??
                                'UG', // Optional: Set an initial country code
                            onChanged: (phone) {
                              setState(() {
                                _nextOfKinPhoneNumber = phone;
                              });
                            },
                            onCountryChanged: (country) {},
                            validator: (value) {
                              if (_nextOfKinPhoneNumber == null ||
                                  _nextOfKinPhoneNumber!.number.isEmpty) {
                                return 'Please enter your next of kin\'s phone number';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _nextOfKinEmailController,
                            decoration: const InputDecoration(
                                labelText: 'Next of Kin\'s Email'),
                            validator: (value) {
                              return validateEmail(value);
                            },
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
              ],
            ),
          ),
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                ElevatedButton(
                  onPressed: currentIndex > 0 ? _goToPreviousPage : null,
                  child: const Icon(Icons.arrow_back),
                ),
                ElevatedButton(
                  onPressed: () {
                    if (currentIndex < _numPages - 1) {
                      print('first form');
                      if (_formKeys[currentIndex].currentState!.validate()) {
                        _goToNextPage();
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                              content: Text(
                                  'Please fill out all fields on this page')),
                        );
                      }
                    } else {
                      print('secon-form');
                      _saveToDb(currentIndex);
                      _saveProfileToServer();
                      _navigateToForm1(context);
                    }
                  },
                  child: const Icon(Icons.arrow_forward),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  @override
  void dispose() {
    _pageController.dispose();
    _fullNameController.dispose();
    _countryController.dispose();
    _countryOfResidenceController.dispose();
    _cityController.dispose();
    _occupationController.dispose();
    _emailController.dispose();
    _nextOfKinEmailController.dispose();
    _nextOfKinNameController.dispose();
    _nextOfKinRelationshipController.dispose();
    _page4Controller1.dispose();
    _page4Controller2.dispose();
//    db.dispose();
    super.dispose();
  }
}

class Form1 extends StatefulWidget {
  const Form1({super.key, this.demographicsData});

  final Map<String, dynamic>? demographicsData;

  @override
  State<Form1> createState() => _Form1State();
}

class _Form1State extends State<Form1> {
  final GlobalKey<FormState> _form1Key = GlobalKey<FormState>();
  final TextEditingController _form1Controller1 = TextEditingController();
  final TextEditingController _form1Controller2 = TextEditingController();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Form 1')),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _form1Key,
          child: Column(
            children: [
              const Text('Form 1 Entries'),
              Padding(
                padding: const EdgeInsets.symmetric(vertical: 8.0),
                child: TextFormField(
                  controller: _form1Controller1,
                  decoration:
                      const InputDecoration(labelText: 'Form 1 Entry 1'),
                  validator: (value) {
                    if (value == null || value.isEmpty) {
                      return 'Please enter some text';
                    }
                    return null;
                  },
                ),
              ),
              Padding(
                padding: const EdgeInsets.symmetric(vertical: 8.0),
                child: TextFormField(
                  controller: _form1Controller2,
                  decoration:
                      const InputDecoration(labelText: 'Form 1 Entry 2'),
                  validator: (value) {
                    if (value == null || value.isEmpty) {
                      return 'Please enter some text';
                    }
                    return null;
                  },
                ),
              ),
              const SizedBox(height: 20),
            ],
          ),
        ),
      ),
      bottomNavigationBar: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            ElevatedButton(
              onPressed: () => Navigator.pop(context),
              child: const Icon(Icons.arrow_back),
            ),
            ElevatedButton(
              onPressed: () {
                if (_form1Key.currentState!.validate()) {
                  final form1Data = {
                    'entry1': _form1Controller1.text,
                    'entry2': _form1Controller2.text,
                  };
                  Navigator.push(
                    context,
                    MaterialPageRoute(
                      builder: (context) => Form2(
                        demographicsData: widget.demographicsData,
                        form1Data: form1Data,
                      ),
                    ),
                  );
                } else {
                  ScaffoldMessenger.of(context).showSnackBar(
                    const SnackBar(
                        content:
                            Text('Please fill out all fields on this form')),
                  );
                }
              },
              child: const Icon(Icons.arrow_forward),
            ),
          ],
        ),
      ),
    );
  }

  @override
  void dispose() {
    _form1Controller1.dispose();
    _form1Controller2.dispose();
    super.dispose();
  }
}

class Form2 extends StatefulWidget {
  const Form2({super.key, this.demographicsData, this.form1Data});

  final Map<String, dynamic>? demographicsData;
  final Map<String, String>? form1Data;

  @override
  State<Form2> createState() => _Form2State();
}

class _Form2State extends State<Form2> {
  final GlobalKey<FormState> _form2Key = GlobalKey<FormState>();
  final TextEditingController _form2Controller1 = TextEditingController();
  final TextEditingController _form2Controller2 = TextEditingController();

  void _submitAllForms(BuildContext context) {
    if (_form2Key.currentState!.validate()) {
      final form2Data = {
        'entry1': _form2Controller1.text,
        'entry2': _form2Controller2.text,
      };

      final combinedFormData = {
        'demographics': widget.demographicsData,
        'form1': widget.form1Data,
        'form2': form2Data,
      };
      print('Combined Form Data: $combinedFormData');

      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Processing All Data')),
      );
      // You might want to navigate to a success screen here
    } else {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
            content: Text('Please fill out all fields on this form')),
      );
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Form 2')),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _form2Key,
          child: Column(
            children: [
              const Text('Form 2 Entries'),
              Padding(
                padding: const EdgeInsets.symmetric(vertical: 8.0),
                child: TextFormField(
                  controller: _form2Controller1,
                  decoration:
                      const InputDecoration(labelText: 'Form 2 Entry 1'),
                  validator: (value) {
                    if (value == null || value.isEmpty) {
                      return 'Please enter some text';
                    }
                    return null;
                  },
                ),
              ),
              Padding(
                padding: const EdgeInsets.symmetric(vertical: 8.0),
                child: TextFormField(
                  controller: _form2Controller2,
                  decoration:
                      const InputDecoration(labelText: 'Form 2 Entry 2'),
                  validator: (value) {
                    if (value == null || value.isEmpty) {
                      return 'Please enter some text';
                    }
                    return null;
                  },
                ),
              ),
              const SizedBox(height: 20),
            ],
          ),
        ),
      ),
      bottomNavigationBar: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            ElevatedButton(
              onPressed: () => Navigator.pop(context),
              child: const Icon(Icons.arrow_back),
            ),
            ElevatedButton(
              onPressed: () => _submitAllForms(context),
              child: const Text('Finish'),
            ),
          ],
        ),
      ),
    );
  }

  @override
  void dispose() {
    _form2Controller1.dispose();
    _form2Controller2.dispose();
    super.dispose();
  }
}

class Home extends StatelessWidget {
  const Home({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Form 2')),
      body: const Text('hello'),
    );
  }
}
