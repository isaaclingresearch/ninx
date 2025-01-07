part of '../main.dart';

class MedicationData {
  final String name;
  final String reason;
  final String methodOfStarting;
  final String dosage; // Changed dosage to String
  final String frequency;
  final DateTime startDate;
  final int duration; // Duration in days
  final String complaints;

  MedicationData({
    required this.name,
    required this.reason,
    required this.methodOfStarting,
    required this.dosage,
    required this.frequency,
    required this.startDate,
    required this.duration,
    required this.complaints,
  });

  Map<String, dynamic> toMap() {
    return {
      'name': name,
      'reason': reason,
      'method-of-starting': methodOfStarting,
      'dosage': dosage, // Now a String
      'frequency': frequency,
      'date-of-start': DateFormat('yyyy-MM-dd').format(startDate),
      'duration-of-taking': duration,
      'complaints-about-drug': complaints,
    };
  }

  static void saveToDb(List<MedicationData> data) {
    DbHandle db = DbHandle();
    db.setMedicationHistory(
        db.getCurrentUserId(), jsonEncode(data.map((e) => e.toMap()).toList()));
    db.close();
  }

  static Future<void> saveToServer(List<MedicationData> data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveMedication(
          db.getCurrentUserId()!, jsonEncode(data.map((e) => e.toMap()).toList()));
    } catch (error) {
      print('Error saving to server: $error');
      rethrow;
    } finally {
      api.close();
      db.close();
    }
  }
}

class MedicationsForm extends StatefulWidget {
  const MedicationsForm({super.key});

  @override
  State<MedicationsForm> createState() => _MedicationsFormState();
}

class _MedicationsFormState extends State<MedicationsForm> {
  final GlobalKey<FormState> _key = GlobalKey<FormState>();
  final GlobalKey<FormState> _detailsKey = GlobalKey<FormState>();
  final TextEditingController _countController = TextEditingController();
  int _currentIndex = 0;
  int _medicationCount = 0;
  final PageController _pageController = PageController(initialPage: 0);
  List<List<TextEditingController>> _detailsControllers = [];
  List<String?> _selectedMethods = []; // Store selected methods for dropdown
  late DbHandle db;
  List<DateTime?> _startDates = [];
  List<int?> _durationValues = [];

  final List<String> _methodOptions = ['Prescribed', 'OTC', 'Self-prescription'];

  @override
  void initState() {
    super.initState();
    db = DbHandle();
    _countController.addListener(_onCountChanged);
  }

  void _onCountChanged() {
    if (_countController.text.isNotEmpty) {
      int count = int.tryParse(_countController.text) ?? 0;
      if (count != _medicationCount) {
        setState(() {
          _medicationCount = count;
          _detailsControllers = List.generate(
              _medicationCount,
              (_) => List.generate(
                  6, (_) => TextEditingController())); // 6 controllers now
          _startDates = List.generate(_medicationCount, (_) => null);
          _selectedMethods = List.generate(_medicationCount, (_) => null);
          _durationValues = List.generate(_medicationCount, (_) => null);
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

  void _removeMedication(int index, BuildContext context) {
    setState(() {
      for (var controller in _detailsControllers[index]) {
        controller.dispose();
      }
      _detailsControllers.removeAt(index);
      _selectedMethods.removeAt(index); // Remove selected method
      _medicationCount--;
      _startDates.removeAt(index);
      _durationValues.removeAt(index);

      _countController.text = _medicationCount.toString();
    });
    if (_medicationCount == 0) {
      _navigateToNextForm(context);
    }
  }

  void _addMedication() {
    setState(() {
      _medicationCount++;
      _detailsControllers
          .add(List.generate(6, (_) => TextEditingController())); // 6 controllers
      _selectedMethods.add(null); // Initialize dropdown value
      _startDates.add(null);
      _durationValues.add(null);
      _countController.text = _medicationCount.toString();
    });
  }

  Future<void> _selectDate(BuildContext context, int index) async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate: _startDates[index] ?? DateTime.now(),
      firstDate: DateTime(1900),
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
      for (int i = 0; i < _medicationCount; i++) ...[
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            Text('Medication ${i + 1}'),
            IconButton(
              icon: Icon(Icons.delete),
              onPressed: () => _removeMedication(i, context),
            ),
          ],
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][0],
            decoration: const InputDecoration(labelText: 'Name of Medication'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the name of the medication.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][1],
            decoration: const InputDecoration(
                labelText: 'Reason for taking (Indication)'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the reason for taking the medication';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: DropdownButtonFormField<String>(
            value: _selectedMethods[i],
            decoration:
                const InputDecoration(labelText: 'Method of Starting'),
            items: _methodOptions.map((String method) {
              return DropdownMenuItem<String>(
                value: method,
                child: Text(method),
              );
            }).toList(),
            onChanged: (newValue) {
              setState(() {
                _selectedMethods[i] = newValue;
              });
            },
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please select a method of starting';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][2],
            decoration:
                const InputDecoration(labelText: 'Dosage (e.g. 500mg)'), // Dosage is now a string
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the dosage';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][3],
            decoration: const InputDecoration(
                labelText: 'Frequency (e.g. Daily, twice a day)'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the frequency';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            readOnly: true,
            onTap: () => _selectDate(context, i),
            controller: TextEditingController(
              text: _startDates[i] != null
                  ? DateFormat('yyyy-MM-dd').format(_startDates[i]!)
                  : '',
            ),
            decoration: const InputDecoration(
              labelText: 'When did you start taking it?',
              suffixIcon: Icon(Icons.calendar_today),
            ),
            validator: (value) {
              if (_startDates[i] == null) {
                return 'Please select a start date';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][4],
            keyboardType: TextInputType.number,
            decoration: const InputDecoration(
                labelText: 'Duration of taking (in days)'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the duration';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][5],
            minLines: 2,
            maxLines: 5,
            keyboardType: TextInputType.multiline,
            decoration: const InputDecoration(
                labelText: 'Complaints about the drug'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter any complaints or N/A';
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

  void _navigateToNextForm(BuildContext context) {
    // Replace this with your actual navigation logic to the next form
    // Navigator.push(context, MaterialPageRoute(builder: (context) => NextForm()));
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          title: const Text('Medications'),
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
                            labelText: 'How many medications do you take?'),
                        validator: (value) {
                          if (value == null || value.isEmpty) {
                            return 'Please enter the number of medications.';
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
                    onPressed: _addMedication,
                    child: const Icon(Icons.add),
                  ),
                ElevatedButton(
                  onPressed: () async {
                    if (_currentIndex == 0) {
                      if (_key.currentState!.validate()) {
                        setState(() {
                          _medicationCount = int.parse(_countController.text);
                        });
                        if (_medicationCount == 0) {
                          _navigateToNextForm(
                              context); // Replace with your navigation
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
                        List<MedicationData> data = [
                          for (int i = 0; i < _medicationCount; i++) ...[
                            MedicationData(
                              name: _detailsControllers[i][0].text,
                              reason: _detailsControllers[i][1].text,
                              methodOfStarting: _selectedMethods[i]!,
                              dosage: _detailsControllers[i][2]
                                  .text, // Now a String
                              frequency: _detailsControllers[i][3].text,
                              startDate: _startDates[i]!,
                              duration:
                                  int.parse(_detailsControllers[i][4].text),
                              complaints: _detailsControllers[i][5].text,
                            ),
                          ]
                        ];
                        MedicationData.saveToDb(data);
                        try {
                          await MedicationData.saveToServer(data);
                          if (!context.mounted) return;
                          print('Data saved successfully');
                          _navigateToNextForm(
                              context); // Replace with your navigation
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
