part of 'main.dart';

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
  
  void _saveToDb() {
    var data = [
      for (int i = 0; i < _allergyCount; i++) ...[
        {
          'name': _detailsControllers[i][0].text,
          'severity': _severityValues[i],
          'details': _detailsControllers[i][1].text,
          'management': _detailsControllers[i][2].text,
          'date-of-start': DateFormat('yyyy-mm-dd').format(_startDates[i]!),
        }
      ]
    ];
    db.setAllergyHistory(db.getCurrentUserId(), jsonEncode(data));
  }

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
      initialDate:
          _startDates[index] ?? DateTime.now(), // Show today if no date selected
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
            decoration:
                const InputDecoration(labelText: 'Management: How are you treating it?'),
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
          builder: (context) => ChronicDiseasesForm(),
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
                  onPressed: () {
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
                        _saveToDb();
                          _navigateToChronicDiseaseForm(context);
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

class ChronicDiseasesForm extends StatefulWidget {
  const ChronicDiseasesForm({super.key});

  @override
  State<ChronicDiseasesForm> createState() => _ChronicDiseasesFormState();
}

class _ChronicDiseasesFormState extends State<ChronicDiseasesForm> {
  final GlobalKey<FormState> _key = GlobalKey<FormState>();
  final TextEditingController _countController = TextEditingController();
  int _currentIndex = 0;
  final PageController _pageController = PageController(initialPage: 0);

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

  void _navigateToAllergiesForm(BuildContext context) {
    Navigator.push(
        context,
        MaterialPageRoute(
          builder: (context) => AllergiesForm(),
        ));
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          title: const Text('Chronic diseases'),
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
              const Text('page one'),
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
                ElevatedButton(
                  onPressed: () {
                    if (_currentIndex == 0) {
                      print('first form');
                      if (_key.currentState!.validate()) {
                        (_countController.text == '0')
                            ? _navigateToAllergiesForm(context)
                            : _goToNextPage;
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                              content: Text(
                                  'Please fill out all fields on this page')),
                        );
                      }
                    } else {
                      print('secon-form');
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
