import 'package:flutter/material.dart';
import 'dart:ffi';
import 'dart:io';

import 'package:intl/intl.dart'; // For date formatting
import 'package:country_picker/country_picker.dart';
import 'package:intl_phone_field/intl_phone_field.dart';
import 'package:intl_phone_field/phone_number.dart';
import 'package:path/path.dart';
import 'package:sqlite3/open.dart';
import 'package:sqlite3/sqlite3.dart';
import 'package:path_provider/path_provider.dart';

 
void main() async {
  open.overrideFor(OperatingSystem.linux, _openOnLinux);
  final directory = Platform.isIOS ? await getLibraryDirectory() : await getApplicationDocumentsDirectory();
  String path = '${directory.path}cenna.db';
  final db = sqlite3.open(path);
  // create tables
  db.execute('''
    CREATE TABLE IF NOT EXISTS system_variables (
      variable text primary key,
      value text,
      set_date text default CURRENT_TIMESTAMP);

    CREATE TABLE IF NOT EXISTS demographics (
      demographic text primary key,
      value text,
      set_date text default CURRENT_TIMESTAMP);
    ''');
  runApp(const Cenna());
  db.dispose();
}

DynamicLibrary _openOnLinux() {
  final scriptDir = File(Platform.script.toFilePath()).parent;
  final libraryNextToScript = File(join(scriptDir.path, 'sqlite3.so'));
  return DynamicLibrary.open(libraryNextToScript.path);
}

class Cenna extends StatelessWidget {
  const Cenna({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Cenna',
      home: const DemographicsForm(),
    );
  }
}

class DemographicsForm extends StatefulWidget {
  const DemographicsForm({super.key});
  @override
  State<DemographicsForm> createState() => _DemographicsFormState();
}

class _DemographicsFormState extends State<DemographicsForm> {
  int currentIndex = 0;
  final PageController _pageController = PageController(initialPage: 0);
  final int _numPages = 4; // Number of pages
  final List<GlobalKey<FormState>> _formKeys =
      List.generate(4, (_) => GlobalKey<FormState>());

  // Controllers for Page 1
  final TextEditingController _fullNameController = TextEditingController();
  String? _selectedSex; // This will hold "Male" or "Female"
  String? _selectedGender;
  String? _selectedRace;
  String? _selectedEducation;
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
  final TextEditingController _nextOfKinNameController = TextEditingController();
 final TextEditingController _nextOfKinEmailController = TextEditingController();
 final TextEditingController _nextOfKinRelationshipController = TextEditingController();

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

  void _saveToDb(int page) async {
    final directory = Platform.isIOS
        ? await getLibraryDirectory()
        : await getApplicationDocumentsDirectory();
    String path = '${directory.path}cenna.db';
    final db = sqlite3.open(path);
    final query =  db.prepare('''insert into demographics (demographic, value) values (?, ?)''');
    switch (page) {
      case 0:
       query
        ..execute(['full-name',_fullNameController.text])
        ..execute(['sex', _selectedSex])
        ..execute(['date-of-birth', _selectedDate])
        ..execute(['country-of-origin', _selectedCountry])
        ..execute(['level-of-education', _selectedEducation])
        ..execute(['race', _selectedRace])
        ..execute(['gender', _selectedGender]);
      break;
      case 1:
        query
        ..execute(['country-of-origin', _selectedCountry])
        ..execute(['country-of-residence', _selectedCountryOfResidence])
        ..execute(['city', _cityController.text])
        ..execute(['occupation', _occupationController.text]);
        break;
        case 2:
        query
        ..execute(['email', _emailController.text])
        ..execute(['telephone-number', _phoneNumber])
        ..execute(['next-of-kin-name', _nextOfKinNameController.text])
        ..execute(['next-of-kin-email', _nextOfKinEmailController.text])
        ..execute(['next-of-kin-relationship', _nextOfKinRelationshipController.text])
        ..execute(['next-of-kin-telehone', _nextOfKinPhoneNumber]);
    }
    db.dispose();
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
                'next-of-kin-relationship': _nextOfKinRelationshipController.text,
                'next-of-kin-email': _nextOfKinEmailController.text,
                'next-of-kin-phone-number': _nextOfKinPhoneNumber,
                'page41': _page4Controller1.text,
                'page42': _page4Controller2.text
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
      context: context,
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
                _saveToDb(i--);
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
                        Row(mainAxisAlignment: MainAxisAlignment.spaceBetween,
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
                                            .displayName; // Display country name
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
                                        _countryOfResidenceController.text = country
                                            .displayName; // Display country name
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
                            initialCountryCode:
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
                              if (value == null || value.isEmpty) {
                                return 'Please enter your email';
                              }
                              return null;
                            },
                          ),
                        ),
                       Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _nextOfKinNameController,
                            decoration:
                                const InputDecoration(labelText: 'Next of Kin Name'),
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
                            decoration:
                                const InputDecoration(labelText: 'Relationship with next of kin'),
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
                            initialCountryCode:
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
                            decoration:
                                const InputDecoration(labelText: 'Next of Kin\'s Email'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter your next of kin\'s email';
                              }
                              return null;
                            },
                          ),
                        ),
                     ],
                    ),
                  ),
                ),
                // Page 4
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Form(
                    key: _formKeys[3],
                    child: Column(
                      children: [
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            const Text('Page 4 Entries'),
                            TextButton(
                              onPressed: () => _showWhyThisDialog(3),
                              child: const Text('Why this?'),
                            ),
                          ],
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page4Controller1,
                            decoration:
                                const InputDecoration(labelText: 'Entry 1'),
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
                            controller: _page4Controller2,
                            decoration:
                                const InputDecoration(labelText: 'Entry 2'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
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
