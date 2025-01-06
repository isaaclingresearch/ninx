part of 'main.dart';

class AllergyData {
  final String name;
  final double severity;
  final String details;
  final String management;
  final DateTime startDate;

  AllergyData({
    required this.name,
    required this.severity,
    required this.details,
    required this.management,
    required this.startDate,
  });

  Map<String, dynamic> toMap() {
    return {
      'name': name,
      'severity': severity,
      'details': details,
      'management': management,
      'start-date': DateFormat('yyyy-mm-dd').format(startDate),
    };
  }

  static void saveToDb(List<AllergyData> data) {
    DbHandle db = DbHandle();
    db.setAllergyHistory(
        db.getCurrentUserId(), jsonEncode(data.map((e) => e.toMap()).toList()));
    db.close();
  }

  static Future<void> saveToServer(List<AllergyData> data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveAllergy(db.getCurrentUserId()!,
          jsonEncode(data.map((e) => e.toMap()).toList()));
    } catch (error) {
      print('Error saving to server: $error');
      rethrow; // Re-throw the error to propagate it further
    } finally {
      api.close();
      db.close();
    }
  }
}

class AllergiesForm extends StatefulWidget {
  const AllergiesForm({super.key});

  @override
  State<AllergiesForm> createState() => _AllergiesFormState();
}

class _AllergiesFormState extends State<AllergiesForm> {
  final GlobalKey<FormState> _key = GlobalKey<FormState>();
  final GlobalKey<FormState> _detailsKey = GlobalKey<FormState>();
  final TextEditingController _countController = TextEditingController();
  int _currentIndex = 0;
  int _allergyCount = 0;
  final PageController _pageController = PageController(initialPage: 0);
  List<List<TextEditingController>> _detailsControllers = [];
  List<double> _severityValues = []; // List to store severity values
  late DbHandle db;
  List<DateTime?> _startDates = [];

  @override
  void initState() {
    super.initState();
    db = DbHandle();
    _countController.addListener(_onCountChanged);
  }

  void _onCountChanged() {
    if (_countController.text.isNotEmpty) {
      int count = int.tryParse(_countController.text) ?? 0;
      if (count != _allergyCount) {
        setState(() {
          _allergyCount = count;
          _detailsControllers = List.generate(_allergyCount,
              (_) => List.generate(3, (_) => TextEditingController()));
          // Initialize severity values for each allergy
          _severityValues = List.generate(_allergyCount, (_) => 5.0);
          _startDates = List.generate(_allergyCount, (_) => null);
        });
      }
    }
  }

  @override
  void dispose() {
    _countController.dispose();
    _pageController.dispose();
    db.close();
    for (var group in _detailsControllers) {
      for (var controller in group) {
        controller.dispose();
      }
    }
    super.dispose();
  }

  void _removeAllergy(int index, BuildContext context) {
    setState(() {
      // Dispose the controllers for the removed allergy
      for (var controller in _detailsControllers[index]) {
        controller.dispose();
      }

      // Remove the controllers, severity value, and the allergy group
      _detailsControllers.removeAt(index);
      _severityValues.removeAt(index);
      _allergyCount--;
      _startDates.removeAt(index);

      // Update the allergy count in the count controller
      _countController.text = _allergyCount.toString();
    });
    //when all allergies are deleted.
    if (_allergyCount == 0) {
      _navigateToChronicDiseaseForm(context);
    }
  }

  void _addAllergy() {
    setState(() {
      _allergyCount++;
      // Create new controllers for the added allergy
      _detailsControllers.add(List.generate(3, (_) => TextEditingController()));
      // Initialize severity value for the new allergy
      _severityValues.add(5.0);
      // Add a DateTime object
      _startDates.add(null);

      // Update the allergy count in the count controller
      _countController.text = _allergyCount.toString();
    });
  }

  Future<void> _selectDate(BuildContext context, int index) async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate: _startDates[index] ??
          DateTime.now(), // Show today if no date selected
      firstDate: DateTime(1900), // Adjust range as needed
      lastDate: DateTime.now(),
    );

    if (pickedDate != null && pickedDate != _startDates[index]) {
      setState(() {
        _startDates[index] = pickedDate;
      });
    }
  }

  List<Widget> _makeDetailChildren(BuildContext context) {
    return [
      for (int i = 0; i < _allergyCount; i++) ...[
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            Text('Allergy ${i + 1}'),
            IconButton(
              icon: Icon(Icons.delete),
              onPressed: () => _removeAllergy(i, context),
            ),
          ],
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][0],
            decoration: const InputDecoration(
                labelText: 'Allergic to: Forexample Pollen'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter what you are allergic to.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            readOnly: true, // Prevent manual editing
            onTap: () => _selectDate(context, i), // Open date picker on tap
            controller: TextEditingController(
              text: _startDates[i] != null
                  ? DateFormat('yyyy-MM-dd').format(_startDates[i]!)
                  : '', // Format and display date
            ),
            decoration: const InputDecoration(
              labelText: 'When did it start?',
              suffixIcon: Icon(Icons.calendar_today), // Add a calendar icon
            ),
            validator: (value) {
              if (_startDates[i] == null) {
                return 'Please select a date';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text('Severity: ${_severityValues[i].round()}'),
              Slider(
                value: _severityValues[i],
                min: 1.0,
                max: 10.0,
                divisions: 9,
                label: _severityValues[i].round().toString(),
                onChanged: (value) {
                  setState(() {
                    _severityValues[i] = value;
                  });
                },
              ),
            ],
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][1],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration:
                const InputDecoration(labelText: 'Reaction: What happens?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the reaction.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][2],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration: const InputDecoration(
                labelText: 'Management: How are you treating it?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the management.';
              }
              return null;
            },
          ),
        ),
      ]
    ];
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

  void _navigateToChronicDiseaseForm(BuildContext context) {
    Navigator.push(
        context,
        MaterialPageRoute(
          builder: (context) => ChronicDiseaseForm(),
        ));
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          title: const Text('Allergies'),
        ),
        body: Column(children: [
          Expanded(
              child: PageView(
            controller: _pageController,
            physics: const NeverScrollableScrollPhysics(),
            onPageChanged: (i) {
              setState(() {
                _currentIndex = i;
              });
            },
            children: [
              Form(
                  key: _key,
                  child: Column(children: [
                    Padding(
                      padding: const EdgeInsets.symmetric(vertical: 8.0),
                      child: TextFormField(
                        controller: _countController,
                        keyboardType: TextInputType.number,
                        inputFormatters: <TextInputFormatter>[
                          FilteringTextInputFormatter.digitsOnly
                        ],
                        decoration: const InputDecoration(
                            labelText: 'How many allergies do you have?'),
                        validator: (value) {
                          if (value == null || value.isEmpty) {
                            return 'Please enter how many allergies you have.';
                          }
                          return null;
                        },
                      ),
                    ),
                  ])),
              Form(
                  key: _detailsKey,
                  child: Column(
                    children: _makeDetailChildren(context),
                  )),
            ],
          )),
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                ElevatedButton(
                  onPressed: _currentIndex > 0 ? _goToPreviousPage : null,
                  child: const Icon(Icons.arrow_back),
                ),
                if (_currentIndex == 1)
                  ElevatedButton(
                    onPressed: _addAllergy,
                    child: const Icon(Icons.add),
                  ),
                ElevatedButton(
                  onPressed: () async {
                    if (_currentIndex == 0) {
                      if (_key.currentState!.validate()) {
                        setState(() {
                          _allergyCount = int.parse(_countController.text);
                        });
                        if (_allergyCount == 0) {
                          _navigateToChronicDiseaseForm(context);
                        } else {
                          _goToNextPage();
                        }
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                            content:
                                Text('Please fill out all fields on this page'),
                          ),
                        );
                      }
                    } else if (_currentIndex == 1) {
                      if (_detailsKey.currentState!.validate()) {
                        List<AllergyData> data = [
                          for (int i = 0; i < _allergyCount; i++) ...[
                            AllergyData(
                              name: _detailsControllers[i][0].text,
                              severity: _severityValues[i],
                              details: _detailsControllers[i][1].text,
                              management: _detailsControllers[i][2].text,
                              startDate: _startDates[i]!,
                            ),
                          ]
                        ];
                        AllergyData.saveToDb(data);
                        try {
                          await AllergyData.saveToServer(data);
                          if (!context.mounted) return;
                          print('Data saved successfully');
                          _navigateToChronicDiseaseForm(context);
                        } catch (error) {
                          print('Error: $error');
                          ScaffoldMessenger.of(context).showSnackBar(
                            SnackBar(
                              content: Text('Failed to save data: $error'),
                            ),
                          );
                          return; // Prevent navigation on error
                        }
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                            content:
                                Text('Please fill out all fields on this page'),
                          ),
                        );
                      }
                    }
                  },
                  child: const Icon(Icons.arrow_forward),
                ),
              ],
            ),
          ),
        ]));
  }
}

class ChronicDiseaseData {
  final String name;
  final double severity;
  final String details;
  final String management;
  final String complications;
  final DateTime startDate;

  ChronicDiseaseData({
    required this.name,
    required this.severity,
    required this.details,
    required this.management,
    required this.startDate,
    required this.complications,
  });

  Map<String, dynamic> toMap() {
    return {
      'name': name,
      'severity': severity,
      'details': details,
      'management': management,
      'complications': complications,
      'start-date': DateFormat('yyyy-mm-dd').format(startDate),
    };
  }

  static void saveToDb(List<ChronicDiseaseData> data) {
    DbHandle db = DbHandle();
    db.setChronicDiseaseHistory(
        db.getCurrentUserId(), jsonEncode(data.map((e) => e.toMap()).toList()));
    db.close();
  }

  static Future<void> saveToServer(List<ChronicDiseaseData> data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveChronicDisease(db.getCurrentUserId()!,
          jsonEncode(data.map((e) => e.toMap()).toList()));
    } catch (error) {
      print('Error saving to server: $error');
      rethrow; // Re-throw the error to propagate it further
    } finally {
      api.close();
      db.close();
    }
  }
}

class ChronicDiseaseForm extends StatefulWidget {
  const ChronicDiseaseForm({super.key});

  @override
  State<ChronicDiseaseForm> createState() => _ChronicDiseaseFormState();
}

class _ChronicDiseaseFormState extends State<ChronicDiseaseForm> {
  final GlobalKey<FormState> _key = GlobalKey<FormState>();
  final GlobalKey<FormState> _detailsKey = GlobalKey<FormState>();
  final TextEditingController _countController = TextEditingController();
  int _currentIndex = 0;
  int _chronicDiseaseCount = 0;
  final PageController _pageController = PageController(initialPage: 0);
  List<List<TextEditingController>> _detailsControllers = [];
  List<double> _severityValues = []; // List to store severity values
  late DbHandle db;
  List<DateTime?> _startDates = [];

  @override
  void initState() {
    super.initState();
    db = DbHandle();
    _countController.addListener(_onCountChanged);
  }

  void _onCountChanged() {
    if (_countController.text.isNotEmpty) {
      int count = int.tryParse(_countController.text) ?? 0;
      if (count != _chronicDiseaseCount) {
        setState(() {
          _chronicDiseaseCount = count;
          _detailsControllers = List.generate(_chronicDiseaseCount,
              (_) => List.generate(4, (_) => TextEditingController()));
          // Initialize severity values for each chronicDisease
          _severityValues = List.generate(_chronicDiseaseCount, (_) => 5.0);
          _startDates = List.generate(_chronicDiseaseCount, (_) => null);
        });
      }
    }
  }

  @override
  void dispose() {
    _countController.dispose();
    _pageController.dispose();
    db.close();
    for (var group in _detailsControllers) {
      for (var controller in group) {
        controller.dispose();
      }
    }
    super.dispose();
  }

  void _removeChronicDisease(int index, BuildContext context) {
    setState(() {
      // Dispose the controllers for the removed chronicDisease
      for (var controller in _detailsControllers[index]) {
        controller.dispose();
      }

      // Remove the controllers, severity value, and the chronicDisease group
      _detailsControllers.removeAt(index);
      _severityValues.removeAt(index);
      _chronicDiseaseCount--;
      _startDates.removeAt(index);

      // Update the chronicDisease count in the count controller
      _countController.text = _chronicDiseaseCount.toString();
    });
    //when all chronic diseases are deleted.
    if (_chronicDiseaseCount == 0) {
      _navigateToAdmissionForm(context);
    }
  }

  void _addChronicDisease() {
    setState(() {
      _chronicDiseaseCount++;
      // Create new controllers for the added chronicDisease
      _detailsControllers.add(List.generate(4, (_) => TextEditingController()));
      // Initialize severity value for the new chronicDisease
      _severityValues.add(5.0);
      // Add a DateTime object
      _startDates.add(null);

      // Update the chronicDisease count in the count controller
      _countController.text = _chronicDiseaseCount.toString();
    });
  }

  Future<void> _selectDate(BuildContext context, int index) async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate: _startDates[index] ??
          DateTime.now(), // Show today if no date selected
      firstDate: DateTime(1900), // Adjust range as needed
      lastDate: DateTime.now(),
    );

    if (pickedDate != null && pickedDate != _startDates[index]) {
      setState(() {
        _startDates[index] = pickedDate;
      });
    }
  }

  List<Widget> _makeDetailChildren(BuildContext context) {
    return [
      for (int i = 0; i < _chronicDiseaseCount; i++) ...[
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            Text('ChronicDisease ${i + 1}'),
            IconButton(
              icon: Icon(Icons.delete),
              onPressed: () => _removeChronicDisease(i, context),
            ),
          ],
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][0],
            decoration: const InputDecoration(
                labelText: 'Allergic to: Forexample Pollen'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter what you are allergic to.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            readOnly: true, // Prevent manual editing
            onTap: () => _selectDate(context, i), // Open date picker on tap
            controller: TextEditingController(
              text: _startDates[i] != null
                  ? DateFormat('yyyy-MM-dd').format(_startDates[i]!)
                  : '', // Format and display date
            ),
            decoration: const InputDecoration(
              labelText: 'When did it start?',
              suffixIcon: Icon(Icons.calendar_today), // Add a calendar icon
            ),
            validator: (value) {
              if (_startDates[i] == null) {
                return 'Please select a date';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text('Severity: ${_severityValues[i].round()}'),
              Slider(
                value: _severityValues[i],
                min: 1.0,
                max: 10.0,
                divisions: 9,
                label: _severityValues[i].round().toString(),
                onChanged: (value) {
                  setState(() {
                    _severityValues[i] = value;
                  });
                },
              ),
            ],
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][1],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration:
                const InputDecoration(labelText: 'Reaction: What happens?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the reaction.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][2],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration: const InputDecoration(
                labelText: 'Management: How are you treating it?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the management.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][3],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration: const InputDecoration(
                labelText: 'Complications: Any complications?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the complications or none.';
              }
              return null;
            },
          ),
        ),
      ]
    ];
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

  void _navigateToAdmissionForm(BuildContext context) {
    Navigator.push(
        context,
        MaterialPageRoute(
          builder: (context) => ChronicDiseaseForm(),
        ));
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          title: const Text('Chronic Disease'),
        ),
        body: Column(children: [
          Expanded(
              child: PageView(
            controller: _pageController,
            physics: const NeverScrollableScrollPhysics(),
            onPageChanged: (i) {
              setState(() {
                _currentIndex = i;
              });
            },
            children: [
              Form(
                  key: _key,
                  child: Column(children: [
                    Padding(
                      padding: const EdgeInsets.symmetric(vertical: 8.0),
                      child: TextFormField(
                        controller: _countController,
                        keyboardType: TextInputType.number,
                        inputFormatters: <TextInputFormatter>[
                          FilteringTextInputFormatter.digitsOnly
                        ],
                        decoration: const InputDecoration(
                            labelText: 'How many chronic diseases do you have?'),
                        validator: (value) {
                          if (value == null || value.isEmpty) {
                            return 'Please enter how many chronic diseases you have.';
                          }
                          return null;
                        },
                      ),
                    ),
                  ])),
              Form(
                  key: _detailsKey,
                  child: Column(
                    children: _makeDetailChildren(context),
                  )),
            ],
          )),
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                ElevatedButton(
                  onPressed: _currentIndex > 0 ? _goToPreviousPage : null,
                  child: const Icon(Icons.arrow_back),
                ),
                if (_currentIndex == 1)
                  ElevatedButton(
                    onPressed: _addChronicDisease,
                    child: const Icon(Icons.add),
                  ),
                ElevatedButton(
                  onPressed: () async {
                    if (_currentIndex == 0) {
                      if (_key.currentState!.validate()) {
                        setState(() {
                          _chronicDiseaseCount =
                              int.parse(_countController.text);
                        });
                        if (_chronicDiseaseCount == 0) {
                          _navigateToAdmissionForm(context);
                        } else {
                          _goToNextPage();
                        }
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                            content:
                                Text('Please fill out all fields on this page'),
                          ),
                        );
                      }
                    } else if (_currentIndex == 1) {
                      if (_detailsKey.currentState!.validate()) {
                        List<ChronicDiseaseData> data = [
                          for (int i = 0; i < _chronicDiseaseCount; i++) ...[
                            ChronicDiseaseData(
                              name: _detailsControllers[i][0].text,
                              severity: _severityValues[i],
                              details: _detailsControllers[i][1].text,
                              management: _detailsControllers[i][2].text,
                              complications: _detailsControllers[i][3].text,
                              startDate: _startDates[i]!,
                            ),
                          ]
                        ];
                        ChronicDiseaseData.saveToDb(data);
                        try {
                          await ChronicDiseaseData.saveToServer(data);
                          if (!context.mounted) return;
                          print('Data saved successfully');
                          _navigateToAdmissionForm(context);
                        } catch (error) {
                          print('Error: $error');
                          ScaffoldMessenger.of(context).showSnackBar(
                            SnackBar(
                              content: Text('Failed to save data: $error'),
                            ),
                          );
                          return; // Prevent navigation on error
                        }
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                            content:
                                Text('Please fill out all fields on this page'),
                          ),
                        );
                      }
                    }
                  },
                  child: const Icon(Icons.arrow_forward),
                ),
              ],
            ),
          ),
        ]));
  }
}


class AdmissionData {
  final String reason;
  final int durationOfStay;
  final String activitiesDone;
  final String complications;
  final String medications;
  final DateTime dateOfAdmission;

  AdmissionData({
    required this.reason,
    required this.durationOfStay,
    required this.activitiesDone,
    required this.complications,
    required this.medications,
    required this.dateOfAdmission,
  });

  Map<String, dynamic> toMap() {
    return {
      'reason-for-admission': reason,
      'duration-of-stay': durationOfStay,
      'activities-done': activitiesDone,
      'complications': complications,
      'medications': medications,
      'date-of-admission': DateFormat('yyyy-mm-dd').format(dateOfAdmission),
    };
  }

  static void saveToDb(List<AdmissionData> data) {
    DbHandle db = DbHandle();
    db.setAdmissionHistory(
        db.getCurrentUserId(), jsonEncode(data.map((e) => e.toMap()).toList()));
    db.close();
  }

  static Future<void> saveToServer(List<AdmissionData> data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveAdmission(db.getCurrentUserId()!,
          jsonEncode(data.map((e) => e.toMap()).toList()));
    } catch (error) {
      print('Error saving to server: $error');
      rethrow; // Re-throw the error to propagate it further
    } finally {
      api.close();
      db.close();
    }
  }
}

class AdmissionForm extends StatefulWidget {
  const AdmissionForm({super.key});

  @override
  State<AdmissionForm> createState() => _AdmissionFormState();
}

class _AdmissionFormState extends State<AdmissionForm> {
  final GlobalKey<FormState> _key = GlobalKey<FormState>();
  final GlobalKey<FormState> _detailsKey = GlobalKey<FormState>();
  final TextEditingController _countController = TextEditingController();
  int _currentIndex = 0;
  int _admissionCount = 0;
  final PageController _pageController = PageController(initialPage: 0);
  List<List<TextEditingController>> _detailsControllers = [];
  late DbHandle db;
  List<DateTime?> _startDates = [];

  @override
  void initState() {
    super.initState();
    db = DbHandle();
    _countController.addListener(_onCountChanged);
  }

  void _onCountChanged() {
    if (_countController.text.isNotEmpty) {
      int count = int.tryParse(_countController.text) ?? 0;
      if (count != _admissionCount) {
        setState(() {
          _admissionCount = count;
          _detailsControllers = List.generate(_admissionCount,
              (_) => List.generate(5, (_) => TextEditingController()));
          _startDates = List.generate(_admissionCount, (_) => null);
        });
      }
    }
  }

  @override
  void dispose() {
    _countController.dispose();
    _pageController.dispose();
    db.close();
    for (var group in _detailsControllers) {
      for (var controller in group) {
        controller.dispose();
      }
    }
    super.dispose();
  }

  void _removeAdmission(int index, BuildContext context) {
    setState(() {
      // Dispose the controllers for the removed admission
      for (var controller in _detailsControllers[index]) {
        controller.dispose();
      }

      // Remove the controllers, severity value, and the admission group
      _detailsControllers.removeAt(index);
      _admissionCount--;
      _startDates.removeAt(index);

      // Update the admission count in the count controller
      _countController.text = _admissionCount.toString();
    });
    //when all admissions are deleted
    if (_admissionCount == 0) {
      _navigateToAdmissionForm(context);
    }
  }

  void _addAdmission() {
    setState(() {
      _admissionCount++;
      // Create new controllers for the added admission
      _detailsControllers.add(List.generate(5, (_) => TextEditingController()));
      // Initialize severity value for the new admission
      // Add a DateTime object
      _startDates.add(null);

      // Update the admission count in the count controller
      _countController.text = _admissionCount.toString();
    });
  }

  Future<void> _selectDate(BuildContext context, int index) async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate: _startDates[index] ??
          DateTime.now(), // Show today if no date selected
      firstDate: DateTime(1900), // Adjust range as needed
      lastDate: DateTime.now(),
    );

    if (pickedDate != null && pickedDate != _startDates[index]) {
      setState(() {
        _startDates[index] = pickedDate;
      });
    }
  }

  List<Widget> _makeDetailChildren(BuildContext context) {
    return [
      for (int i = 0; i < _admissionCount; i++) ...[
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            Text('Admission ${i + 1}'),
            IconButton(
              icon: Icon(Icons.delete),
              onPressed: () => _removeAdmission(i, context),
            ),
          ],
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][0],
            decoration: const InputDecoration(
                labelText: 'Reason for admission'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter why you were admitted.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            readOnly: true, // Prevent manual editing
            onTap: () => _selectDate(context, i), // Open date picker on tap
            controller: TextEditingController(
              text: _startDates[i] != null
                  ? DateFormat('yyyy-MM-dd').format(_startDates[i]!)
                  : '', // Format and display date
            ),
            decoration: const InputDecoration(
              labelText: 'When were you admitted?',
              suffixIcon: Icon(Icons.calendar_today), // Add a calendar icon
            ),
            validator: (value) {
              if (_startDates[i] == null) {
                return 'Please select a date';
              }
              return null;
            },
          ),
        ),
                            Padding(
                      padding: const EdgeInsets.symmetric(vertical: 8.0),
                      child: TextFormField(
                        controller: _detailsControllers[i][1],
                        keyboardType: TextInputType.number,
                        inputFormatters: <TextInputFormatter>[
                          FilteringTextInputFormatter.digitsOnly
                        ],
                        decoration: const InputDecoration(
                            labelText: 'How many days did you stay?'),
                        validator: (value) {
                          if (value == null || value.isEmpty) {
                            return 'Please enter the number of days you stayed.';
                          }
                          return null;
                        },
                      ),
                    ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][2],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration:
                const InputDecoration(labelText: 'What activities were done to you?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the activities that were done during your stay.';
              }
              return null;
            },
          ),
        ),
                Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][3],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration: const InputDecoration(
                labelText: 'What medicine were you given?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the medicine you were given during your stay.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][4],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration: const InputDecoration(
                labelText: 'Complications: Any complications?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the complications or none.';
              }
              return null;
            },
          ),
        ),
      ]
    ];
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

  void _navigateToAdmissionForm(BuildContext context) {
    Navigator.push(
        context,
        MaterialPageRoute(
          builder: (context) => AdmissionForm(),
        ));
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          title: const Text('Admissions'),
        ),
        body: Column(children: [
          Expanded(
              child: PageView(
            controller: _pageController,
            physics: const NeverScrollableScrollPhysics(),
            onPageChanged: (i) {
              setState(() {
                _currentIndex = i;
              });
            },
            children: [
              Form(
                  key: _key,
                  child: Column(children: [
                    Padding(
                      padding: const EdgeInsets.symmetric(vertical: 8.0),
                      child: TextFormField(
                        controller: _countController,
                        keyboardType: TextInputType.number,
                        inputFormatters: <TextInputFormatter>[
                          FilteringTextInputFormatter.digitsOnly
                        ],
                        decoration: const InputDecoration(
                            labelText: 'How many times have you been admitted?'),
                        validator: (value) {
                          if (value == null || value.isEmpty) {
                            return 'Please enter the number of times you have been admitted.';
                          }
                          return null;
                        },
                      ),
                    ),
                  ])),
              Form(
                  key: _detailsKey,
                  child: Column(
                    children: _makeDetailChildren(context),
                  )),
            ],
          )),
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                ElevatedButton(
                  onPressed: _currentIndex > 0 ? _goToPreviousPage : null,
                  child: const Icon(Icons.arrow_back),
                ),
                if (_currentIndex == 1)
                  ElevatedButton(
                    onPressed: _addAdmission,
                    child: const Icon(Icons.add),
                  ),
                ElevatedButton(
                  onPressed: () async {
                    if (_currentIndex == 0) {
                      if (_key.currentState!.validate()) {
                        setState(() {
                          _admissionCount =
                              int.parse(_countController.text);
                        });
                        if (_admissionCount == 0) {
                          _navigateToAdmissionForm(context);
                        } else {
                          _goToNextPage();
                        }
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                            content:
                                Text('Please fill out all fields on this page'),
                          ),
                        );
                      }
                    } else if (_currentIndex == 1) {
                      if (_detailsKey.currentState!.validate()) {
                        List<AdmissionData> data = [
                          for (int i = 0; i < _admissionCount; i++) ...[
                            AdmissionData(
                              reason: _detailsControllers[i][0].text,
                              durationOfStay: int.tryParse(_detailsControllers[i][1].text)!, 
                              activitiesDone: _detailsControllers[i][2].text,
                              medications: _detailsControllers[i][3].text,
                              complications: _detailsControllers[i][4].text,
                              dateOfAdmission: _startDates[i]!,
                            ),
                          ]
                        ];
                        AdmissionData.saveToDb(data);
                        try {
                          await AdmissionData.saveToServer(data);
                          if (!context.mounted) return;
                          print('Data saved successfully');
                          _navigateToAdmissionForm(context);
                        } catch (error) {
                          print('Error: $error');
                          ScaffoldMessenger.of(context).showSnackBar(
                            SnackBar(
                              content: Text('Failed to save data: $error'),
                            ),
                          );
                          return; // Prevent navigation on error
                        }
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                            content:
                                Text('Please fill out all fields on this page'),
                          ),
                        );
                      }
                    }
                  },
                  child: const Icon(Icons.arrow_forward),
                ),
              ],
            ),
          ),
        ]));
  }
}
