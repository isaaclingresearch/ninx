part of 'main.dart';

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

  DbHandle db = DbHandle();
  late String? userId;

  @override
  void initState() {
    super.initState();
    _initialiseValues();
    setState(() {
      userId = db.getCurrentUserId();
    });
  }

  // intialisevalues
  void _initialiseValues() {
    DemographicsData data = db.getInitialDemographics(userId);
    setState(() {
      if (data.fullName != '') {
        _fullNameController.text = data.fullName;
      }
      if (data.sexAtBirth != '') {
        _selectedSex = data.sexAtBirth;
      }
      if (data.gender != '') {
        _selectedGender = data.gender;
      }
      if (data.race != '') {
        _selectedRace = data.race;
      }
      if (data.maritalStatus != '') {
        _selectedMaritalStatus = data.maritalStatus;
      }
      if (data.levelOfEducation != '') {
        _selectedEducation = data.levelOfEducation;
      }
      if (data.dateOfBirth != DateFormat('yyyy-mm-dd').parse('1800-01-01')) {
        _selectedDate = data.dateOfBirth;
      }
      if (data.occupation != '') {
        _occupationController.text = data.occupation;
      }
      if (data.cityOfResidence != '') {
        _cityController.text = data.cityOfResidence;
      }
      if (data.countryOfResidence != '') {
        _selectedCountryOfResidence =
            CountryParser.parseCountryName(data.countryOfResidence);
        _countryOfResidenceController.text = data.countryOfResidence;
      }
      if (data.countryOfOrigin != '') {
        _selectedCountry = CountryParser.parseCountryName(data.countryOfOrigin);
        _countryController.text = data.countryOfOrigin;
      }
      if (data.telephoneNumber != '') {
        _phoneNumber = PhoneNumber.fromCompleteNumber(
            completeNumber: data.telephoneNumber);
      }
      if (data.email != '') {
        _emailController.text = data.email;
      }
      if (data.nextOfKin.name != '') {
        _nextOfKinNameController.text = data.nextOfKin.name;
      }
      if (data.nextOfKin.telephoneNumber != '') {
        _nextOfKinPhoneNumber = PhoneNumber.fromCompleteNumber(
            completeNumber: data.nextOfKin.telephoneNumber);
      }
      if (data.nextOfKin.relationship != '') {
        _nextOfKinRelationshipController.text = data.nextOfKin.relationship;
      }
      if (data.nextOfKin.email != '') {
        _nextOfKinEmailController.text = data.nextOfKin.email;
      }
    });
  }

  // this function will check if data has been saved and then it will skip to the page without data.
  void _skipToPage() {
    if (db.getDemographic(userId, 'full-name') == '') {
      return;
    }
    if (db.getDemographic(userId, 'occupation') != '') {
      setState(() {
        currentIndex = 2;
      });
    }
  }

  void _saveToDb(int page) async {
    print('page: $page\n\n');
    // save the user to allow progress to continue
    db.setCurrentUserId(userId);
    switch (page) {
      case 0:
        db.setDemographics(userId, {
          'full-name': _fullNameController.text,
          'gender': _selectedGender,
          'sex': _selectedSex,
          'date-of-birth': DateFormat('yyyy-MM-dd').format(_selectedDate!),
          'marital-status': _selectedMaritalStatus,
          'level-of-education': _selectedEducation,
          'race': _selectedRace,
        });
        break;
      case 1:
        db.setDemographics(userId, {
          'country-of-origin': _selectedCountry?.name,
          'country-of-residence': _selectedCountryOfResidence?.name,
          'city-of-residence': _cityController.text,
          'occupation': _occupationController.text,
        });
        break;
      case 2:
        db.setDemographics(userId, {
          'email': _emailController.text,
          'telephone-number': _phoneNumber?.completeNumber,
          'next-of-kin-name': _nextOfKinNameController.text,
          'next-of-kin-email': _nextOfKinEmailController.text,
          'next-of-kin-relationship': _nextOfKinRelationshipController.text,
          'next-of-kin-telephone-number': _nextOfKinPhoneNumber?.completeNumber,
        });
    }
  }


  void _saveProfileToServer() async {
    DemographicsData data = DemographicsData(
        fullName: _fullNameController.text,
        sexAtBirth: _selectedSex!,
        dateOfBirth: _selectedDate!,
        maritalStatus: _selectedMaritalStatus!,
        levelOfEducation: _selectedEducation!,
        race: _selectedRace!,
        gender: _selectedGender!,
        countryOfOrigin: _selectedCountry?.name ?? '',
        countryOfResidence: _selectedCountryOfResidence?.name ?? '',
        cityOfResidence: _cityController.text,
        occupation: _occupationController.text,
        email: _emailController.text,
        telephoneNumber: _phoneNumber?.completeNumber ?? '',
        nextOfKin: NextOfKinData(
            name: _nextOfKinNameController.text,
            email: _nextOfKinEmailController.text,
            relationship: _nextOfKinRelationshipController.text,
            telephoneNumber: _nextOfKinPhoneNumber?.completeNumber ?? ''));

    var client = http.Client();
    try {
      var response = await client.post(
          Uri.https('cenna:8443', '/save-user-profile'),
          body: {'profile-json': data.toJson()});
      print(response.body);
      var id = jsonDecode(response.body)['user-id'];
      // when the id is returned, save it and the name to registered-users table.
      // then save the data to the registered-profiles table
      // then redirect to home.
      data.saveToDb(userId);
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
    db.close();
    super.dispose();
  }
}
