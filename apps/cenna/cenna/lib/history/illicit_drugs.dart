part of '../main.dart';

class IllicitDrugHistory {
  final bool everUsed;
  final bool currentlyUsing;
  final List<String> types; // e.g., Marijuana, Cocaine, Heroin, etc.
  final String? frequency; // e.g., Daily, Weekly, Monthly
  final int? yearsUsing;
  final int? quitYear; // Year they quit (if applicable)
  final String? sideEffects; // Any notable side effects experienced

  IllicitDrugHistory({
    required this.everUsed,
    required this.currentlyUsing,
    this.types,
    this.frequency,
    this.yearsUsing,
    this.quitYear,
    this.sideEffects,
  });

  Map<String, dynamic> toMap() {
    return {
      'everUsed': everUsed,
      'currentlyUsing': currentlyUsing,
      'types': types,
      'frequency': frequency,
      'yearsUsing': yearsUsing,
      'quitYear': quitYear,
      'sideEffects': sideEffects,
    };
  }

  // Convert a Map (from JSON) to an IllicitDrugHistory object
  static IllicitDrugHistory fromMap(Map<String, dynamic> map) {
    return IllicitDrugHistory(
      everUsed: map['everUsed'],
      currentlyUsing: map['currentlyUsing'],
      types: List<String>.from(map['types'] ?? []),
      frequency: map['frequency'],
      yearsUsing: map['yearsUsing'],
      quitYear: map['quitYear'],
      sideEffects: map['sideEffects'],
    );
  }

  // Serialize a list of IllicitDrugHistory objects to JSON
  static String serializeList(List<IllicitDrugHistory> histories) {
    return jsonEncode(histories.map((history) => history.toMap()).toList());
  }

  // Deserialize a JSON string to a list of IllicitDrugHistory objects
  static List<IllicitDrugHistory> deserializeList(String jsonString) {
    final List<dynamic> list = jsonDecode(jsonString);
    return list.map((map) => IllicitDrugHistory.fromMap(map)).toList();
  }

  // Save to local database
  static void saveToDb(IllicitDrugHistory data) {
    DbHandle db = DbHandle();
    db.setIllicitDrugHistory(db.getCurrentUserId(), jsonEncode(data.toMap()));
    db.close();
  }

  // Save to server
  static Future<void> saveToServer(IllicitDrugHistory data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveIllicitDrugHistory(
          db.getCurrentUserId()!, jsonEncode(data.toMap()));
    } catch (error) {
      print('Error saving illicit drug history to server: $error');
      rethrow;
    } finally {
      api.close();
      db.close();
    }
  }
}

class IllicitDrugForm extends StatefulWidget {
  const IllicitDrugForm({super.key});

  @override
  State<IllicitDrugForm> createState() => _IllicitDrugFormState();
}

class _IllicitDrugFormState extends State<IllicitDrugForm> {
  final _formKeys = List.generate(3, (_) => GlobalKey<FormState>());
  final PageController _pageController = PageController();
  int _currentPage = 0;

  // Illicit Drug Controllers
  bool _everUsedDrugs = false;
  bool _currentlyUsingDrugs = false;
  List<String> _selectedDrugTypes = [];
  final TextEditingController _drugFrequencyController =
      TextEditingController();
  final TextEditingController _drugYearsUsingController =
      TextEditingController();
  final TextEditingController _drugQuitYearController =
      TextEditingController();
  final TextEditingController _drugSideEffectsController =
      TextEditingController();

  // Common drug types
  final List<String> _drugTypes = [
    'Marijuana',
    'Cocaine',
    'Heroin',
    'Methamphetamine',
    'Ecstasy (MDMA)',
    'LSD',
    'Prescription Opioids',
    'Other'
  ];

  @override
  void dispose() {
    _pageController.dispose();
    _drugFrequencyController.dispose();
    _drugYearsUsingController.dispose();
    _drugQuitYearController.dispose();
    _drugSideEffectsController.dispose();
    super.dispose();
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

  void _navigateToPhysicalActivityForm(BuildContext context) {
    Navigator.push(
      context,
      MaterialPageRoute(
        builder: (context) =>
            PhysicalActivityForm(), // Replace with your next form
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Illicit Drug History'),
      ),
      body: Column(
        children: [
          Expanded(
            child: PageView(
              controller: _pageController,
              physics: const NeverScrollableScrollPhysics(),
              onPageChanged: (index) {
                setState(() {
                  _currentPage = index;
                });
              },
              children: [
                _buildDrugStatusForm(),
                _buildDrugDetailsForm(),
                if (_everUsedDrugs && !_currentlyUsingDrugs)
                  _buildDrugQuitForm(),
              ],
            ),
          ),
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                ElevatedButton(
                  onPressed: _currentPage > 0 ? _goToPreviousPage : null,
                  child: const Icon(Icons.arrow_back),
                ),
                ElevatedButton(
                  onPressed: () async {
                    if (_formKeys[_currentPage].currentState!.validate()) {
                      if (_currentPage == 0) {
                        if (_everUsedDrugs) {
                          _goToNextPage();
                        } else {
                          _saveAndNavigate(context);
                        }
                      } else if (_currentPage == 1) {
                        if (_currentlyUsingDrugs) {
                          _saveAndNavigate(context);
                        } else {
                          _currentPage = 2;
                          _goToNextPage();
                        }
                      } else if (_currentPage == 2) {
                        _saveAndNavigate(context);
                      }
                    } else {
                      ScaffoldMessenger.of(context).showSnackBar(
                        const SnackBar(
                          content:
                              Text('Please fill out all required fields.'),
                        ),
                      );
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

  Widget _buildDrugStatusForm() {
    return Form(
      key: _formKeys[0],
      child: SingleChildScrollView(
        child: Column(
          children: [
            const Padding(
              padding: EdgeInsets.all(8.0),
              child: Text('Illicit Drug Use',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
            ),
            SwitchListTile(
              title: const Text('Have you ever used illicit drugs?'),
              value: _everUsedDrugs,
              onChanged: (value) {
                setState(() {
                  _everUsedDrugs = value;
                  if (!value) {
                    _currentlyUsingDrugs = false;
                  }
                });
              },
            ),
            if (_everUsedDrugs)
              SwitchListTile(
                title: const Text('Currently Using Drugs?'),
                value: _currentlyUsingDrugs,
                onChanged: (value) {
                  setState(() {
                    _currentlyUsingDrugs = value;
                  });
                },
              ),
          ],
        ),
      ),
    );
  }

  Widget _buildDrugDetailsForm() {
    return Form(
      key: _formKeys[1],
      child: SingleChildScrollView(
        child: Column(
          children: [
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  const Text('Types of Drugs Used (Select Multiple)'),
                  ..._drugTypes.map((type) {
                    return CheckboxListTile(
                      title: Text(type),
                      value: _selectedDrugTypes.contains(type),
                      onChanged: (bool? value) {
                        setState(() {
                          if (value != null) {
                            if (value) {
                              _selectedDrugTypes.add(type);
                            } else {
                              _selectedDrugTypes.remove(type);
                            }
                          }
                        });
                      },
                    );
                  }).toList(),
                ],
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: DropdownButtonFormField<String>(
                decoration: const InputDecoration(labelText: 'Frequency'),
                value: _drugFrequencyController.text.isNotEmpty
                    ? _drugFrequencyController.text
                    : null,
                items: [
                  'Daily',
                  'Multiple times a week',
                  'Weekly or Monthly',
                  'More than Monthly'
                ]
                    .map((frequency) => DropdownMenuItem(
                          value: frequency,
                          child: Text(frequency),
                        ))
                    .toList(),
                onChanged: (value) =>
                    setState(() => _drugFrequencyController.text = value!),
                validator: (value) => value == null || value.isEmpty
                    ? 'Please select frequency'
                    : null,
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _drugYearsUsingController,
                decoration:
                    const InputDecoration(labelText: 'Years Using'),
                keyboardType: TextInputType.number,
                validator: (value) => value == null || value.isEmpty
                    ? 'Please enter years using'
                    : null,
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _drugSideEffectsController,
                decoration: const InputDecoration(
                    labelText: 'Side Effects (if any)'),
                maxLines: 3,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildDrugQuitForm() {
    return Form(
      key: _formKeys[2],
      child: SingleChildScrollView(
        child: Column(
          children: [
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _drugQuitYearController,
                decoration: const InputDecoration(
                    labelText: 'Year Quit (if applicable)'),
                keyboardType: TextInputType.number,
              ),
            ),
          ],
        ),
      ),
    );
  }

  void _saveAndNavigate(BuildContext context) {
    IllicitDrugHistory drugHistory = IllicitDrugHistory(
      everUsed: _everUsedDrugs,
      currentlyUsing: _currentlyUsingDrugs,
      types: _selectedDrugTypes,
      frequency: _drugFrequencyController.text,
      yearsUsing: int.tryParse(_drugYearsUsingController.text),
      quitYear: int.tryParse(_drugQuitYearController.text),
      sideEffects: _drugSideEffectsController.text,
    );

    IllicitDrugHistory.saveToDb(drugHistory);
    IllicitDrugHistory.saveToServer(drugHistory).then((_) {
      if (!context.mounted) return;
      print('Illicit drug history data saved successfully');
      _navigateToPhysicalActivityForm(context);
    }).catchError((error) {
      if (!context.mounted) return;
      print('Error: $error');
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text('Failed to save data: $error'),
        ),
      );
    });
  }
}
