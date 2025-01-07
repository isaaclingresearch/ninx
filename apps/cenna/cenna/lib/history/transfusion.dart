part of '../main.dart';

class TransfusionData {
  final String reason;
  final String transfusedWith;
  final String complications;
  final DateTime dateOfTransfusion;

  TransfusionData({
    required this.reason,
    required this.transfusedWith,
    required this.complications,
    required this.dateOfTransfusion,
  });

  Map<String, dynamic> toMap() {
    return {
      'reason-for-transfusion': reason,
      'complications': complications,
      'transfused-with': transfusedWith,
      'date-of-admission': DateFormat('yyyy-MM-dd').format(dateOfTransfusion),
    };
  }

  static void saveToDb(List<TransfusionData> data) {
    DbHandle db = DbHandle();
    db.setTransfusionHistory(
        db.getCurrentUserId(), jsonEncode(data.map((e) => e.toMap()).toList()));
    db.close();
  }

  static Future<void> saveToServer(List<TransfusionData> data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveTransfusion(db.getCurrentUserId()!,
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

class TransfusionForm extends StatefulWidget {
  const TransfusionForm({super.key});

  @override
  State<TransfusionForm> createState() => _TransfusionFormState();
}

class _TransfusionFormState extends State<TransfusionForm> {
  final GlobalKey<FormState> _key = GlobalKey<FormState>();
  final GlobalKey<FormState> _detailsKey = GlobalKey<FormState>();
  final TextEditingController _countController = TextEditingController();
  int _currentIndex = 0;
  int _admissionCount = 0;
  final PageController _pageController = PageController(initialPage: 0);
  List<List<TextEditingController>> _detailsControllers = [];
  List<String?> _transfusedWithOptions =
      []; // Store selected values for dropdown
  late DbHandle db;
  List<DateTime?> _transfusionDates = [];

  final List<String> _transfusionOptions = [
    'Blood',
    'Fresh Frozen Plasma',
    'Platelets'
  ];

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
              (_) => List.generate(2, (_) => TextEditingController()));
          _transfusionDates = List.generate(_admissionCount, (_) => null);
          _transfusedWithOptions = List.generate(
              _admissionCount, (_) => null); // Initialize dropdown values
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

  void _removeTransfusion(int index, BuildContext context) {
    setState(() {
      // Dispose the controllers for the removed admission
      for (var controller in _detailsControllers[index]) {
        controller.dispose();
      }

      // Remove the controllers, selected value, date, and the admission group
      _detailsControllers.removeAt(index);
      _transfusedWithOptions.removeAt(index);
      _transfusionDates.removeAt(index);
      _admissionCount--;

      // Update the admission count in the count controller
      _countController.text = _admissionCount.toString();
    });
    //when all admissions are deleted
    if (_admissionCount == 0) {
      _navigateToTrafficAccidentForm(context);
    }
  }

  void _addTransfusion() {
    setState(() {
      _admissionCount++;
      // Create new controllers for the added admission
      _detailsControllers.add(List.generate(2, (_) => TextEditingController()));
      // Initialize selected value for the new admission
      _transfusedWithOptions.add(null);
      // Add a DateTime object
      _transfusionDates.add(null);

      // Update the admission count in the count controller
      _countController.text = _admissionCount.toString();
    });
  }

  Future<void> _selectDate(BuildContext context, int index) async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate: _transfusionDates[index] ??
          DateTime.now(), // Show today if no date selected
      firstDate: DateTime(1900), // Adjust range as needed
      lastDate: DateTime.now(),
    );

    if (pickedDate != null && pickedDate != _transfusionDates[index]) {
      setState(() {
        _transfusionDates[index] = pickedDate;
      });
    }
  }

  List<Widget> _makeDetailChildren(BuildContext context) {
    return [
      for (int i = 0; i < _admissionCount; i++) ...[
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            Text('Transfusion ${i + 1}'),
            IconButton(
              icon: Icon(Icons.delete),
              onPressed: () => _removeTransfusion(i, context),
            ),
          ],
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][0],
            decoration:
                const InputDecoration(labelText: 'Reason for transfusion'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter why you were transfused.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: DropdownButtonFormField<String>(
            value: _transfusedWithOptions[i],
            decoration: const InputDecoration(labelText: 'Transfused with'),
            items: _transfusionOptions.map((String option) {
              return DropdownMenuItem<String>(
                value: option,
                child: Text(option),
              );
            }).toList(),
            onChanged: (newValue) {
              setState(() {
                _transfusedWithOptions[i] = newValue;
              });
            },
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please select what you were transfused with';
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
              text: _transfusionDates[i] != null
                  ? DateFormat('yyyy-MM-dd').format(_transfusionDates[i]!)
                  : '', // Format and display date
            ),
            decoration: const InputDecoration(
              labelText: 'When were you transfused?',
              suffixIcon: Icon(Icons.calendar_today), // Add a calendar icon
            ),
            validator: (value) {
              if (_transfusionDates[i] == null) {
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

  void _navigateToTrafficAccidentForm(BuildContext context) {
    Navigator.push(
        context,
        MaterialPageRoute(
          builder: (context) => TrafficAccidentForm(),
        ));
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          title: const Text('Transfusions'),
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
                            labelText:
                                'How many times have you been transfused?'),
                        validator: (value) {
                          if (value == null || value.isEmpty) {
                            return 'Please enter the number of times you have been transfused.';
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
                    onPressed: _addTransfusion,
                    child: const Icon(Icons.add),
                  ),
                ElevatedButton(
                  onPressed: () async {
                    if (_currentIndex == 0) {
                      if (_key.currentState!.validate()) {
                        setState(() {
                          _admissionCount = int.parse(_countController.text);
                        });
                        if (_admissionCount == 0) {
                          _navigateToTrafficAccidentForm(context);
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
                        List<TransfusionData> data = [
                          for (int i = 0; i < _admissionCount; i++) ...[
                            TransfusionData(
                              reason: _detailsControllers[i][0].text,
                              transfusedWith: _transfusedWithOptions[i]!,
                              complications: _detailsControllers[i][1].text,
                              dateOfTransfusion: _transfusionDates[i]!,
                            ),
                          ]
                        ];
                        TransfusionData.saveToDb(data);
                        try {
                          await TransfusionData.saveToServer(data);
                          if (!context.mounted) return;
                          print('Data saved successfully');
                          _navigateToTrafficAccidentForm(context);
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
