part of '../main.dart';

class TrafficAccidentData {
  final String description;
  final String complications;
  final DateTime dateOfAccident;

  TrafficAccidentData({
    required this.description,
    required this.complications,
    required this.dateOfAccident,
  });

  Map<String, dynamic> toMap() {
    return {
      'description': description,
      'complications': complications,
      'date-of-accident': DateFormat('yyyy-MM-dd').format(dateOfAccident),
    };
  }

  static void saveToDb(List<TrafficAccidentData> data) {
    DbHandle db = DbHandle();
    db.setTrafficAccidentHistory(
        db.getCurrentUserId(), jsonEncode(data.map((e) => e.toMap()).toList()));
    db.close();
  }

  static Future<void> saveToServer(List<TrafficAccidentData> data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveTrafficAccident(db.getCurrentUserId()!,
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

class TrafficAccidentForm extends StatefulWidget {
  const TrafficAccidentForm({super.key});

  @override
  State<TrafficAccidentForm> createState() => _TrafficAccidentFormState();
}

class _TrafficAccidentFormState extends State<TrafficAccidentForm> {
  final GlobalKey<FormState> _key = GlobalKey<FormState>();
  final GlobalKey<FormState> _detailsKey = GlobalKey<FormState>();
  final TextEditingController _countController = TextEditingController();
  int _currentIndex = 0;
  int _accidentCount = 0; // Changed to _accidentCount
  final PageController _pageController = PageController(initialPage: 0);
  List<List<TextEditingController>> _detailsControllers = [];
  late DbHandle db;
  List<DateTime?> _accidentDates = []; // Changed to _accidentDates

  @override
  void initState() {
    super.initState();
    db = DbHandle();
    _countController.addListener(_onCountChanged);
  }

  void _onCountChanged() {
    if (_countController.text.isNotEmpty) {
      int count = int.tryParse(_countController.text) ?? 0;
      if (count != _accidentCount) {
        setState(() {
          _accidentCount = count;
          _detailsControllers = List.generate(_accidentCount,
              (_) => List.generate(2, (_) => TextEditingController()));
          _accidentDates = List.generate(_accidentCount, (_) => null);
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

  void _removeAccident(int index, BuildContext context) {
    // Changed function name
    setState(() {
      for (var controller in _detailsControllers[index]) {
        controller.dispose();
      }
      _detailsControllers.removeAt(index);
      _accidentDates.removeAt(index); // Remove corresponding date
      _accidentCount--;

      _countController.text = _accidentCount.toString();
    });
    if (_accidentCount == 0) {
      _navigateToTrafficAccidentForm(context); // Corrected navigation
    }
  }

  void _addAccident() {
    // Changed function name
    setState(() {
      _accidentCount++;
      _detailsControllers.add(List.generate(2, (_) => TextEditingController()));
      _accidentDates.add(null); // Initialize date for new entry
      _countController.text = _accidentCount.toString();
    });
  }

  Future<void> _selectDate(BuildContext context, int index) async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate: _accidentDates[index] ?? DateTime.now(),
      firstDate: DateTime(1900),
      lastDate: DateTime.now(),
    );

    if (pickedDate != null && pickedDate != _accidentDates[index]) {
      setState(() {
        _accidentDates[index] = pickedDate;
      });
    }
  }

  List<Widget> _makeDetailChildren(BuildContext context) {
    return [
      for (int i = 0; i < _accidentCount; i++) ...[
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            Text('Accident ${i + 1}'), // Changed label
            IconButton(
              icon: Icon(Icons.delete),
              onPressed: () => _removeAccident(i, context), // Changed function
            ),
          ],
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][0],
            decoration:
                const InputDecoration(labelText: 'Description of Accident'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter a description.';
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
              text: _accidentDates[i] != null
                  ? DateFormat('yyyy-MM-dd').format(_accidentDates[i]!)
                  : '',
            ),
            decoration: const InputDecoration(
              labelText: 'Date of Accident',
              suffixIcon: Icon(Icons.calendar_today),
            ),
            validator: (value) {
              if (_accidentDates[i] == null) {
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
        builder: (context) => const TrafficAccidentForm(),
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Traffic Accidents'),
      ),
      body: Column(
        children: [
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
                  child: Column(
                    children: [
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
                                  'How many traffic accidents have you had?'),
                          validator: (value) {
                            if (value == null || value.isEmpty) {
                              return 'Please enter the number of accidents.';
                            }
                            return null;
                          },
                        ),
                      ),
                    ],
                  ),
                ),
                Form(
                  key: _detailsKey,
                  child: Column(
                    children: _makeDetailChildren(context),
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
                  onPressed: _currentIndex > 0 ? _goToPreviousPage : null,
                  child: const Icon(Icons.arrow_back),
                ),
                if (_currentIndex == 1)
                  ElevatedButton(
                    onPressed: _addAccident, // Changed function
                    child: const Icon(Icons.add),
                  ),
                ElevatedButton(
                  onPressed: () async {
                    if (_currentIndex == 0) {
                      if (_key.currentState!.validate()) {
                        setState(() {
                          _accidentCount = int.parse(_countController.text);
                        });
                        if (_accidentCount == 0) {
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
                        List<TrafficAccidentData> data = [
                          for (int i = 0; i < _accidentCount; i++) ...[
                            TrafficAccidentData(
                              description: _detailsControllers[i][0].text,
                              complications: _detailsControllers[i][1].text,
                              dateOfAccident: _accidentDates[i]!,
                            ),
                          ]
                        ];
                        TrafficAccidentData.saveToDb(data);
                        try {
                          await TrafficAccidentData.saveToServer(data);
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
        ],
      ),
    );
  }
}
