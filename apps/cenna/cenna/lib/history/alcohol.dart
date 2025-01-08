part of '../main.dart';

class AlcoholHistory {
  final bool everConsumed;
  final bool currentlyConsuming;
  final String? type; // e.g., Beer, Wine, Spirits
  final String? frequency; // e.g., Daily, Weekly, Monthly, Socially
  final int? quantity; // e.g., number of drinks per occasion, or units per week
  final int? yearsConsuming;
  final int? quitYear; // Year they quit (if applicable)

  AlcoholHistory({
    required this.everConsumed,
    required this.currentlyConsuming,
    this.type,
    this.frequency,
    this.quantity,
    this.yearsConsuming,
    this.quitYear,
  });

  Map<String, dynamic> toMap() {
    return {
      'everConsumed': everConsumed,
      'currentlyConsuming': currentlyConsuming,
      'type': type,
      'frequency': frequency,
      'quantity': quantity,
      'yearsConsuming': yearsConsuming,
      'quitYear': quitYear,
    };
  }

  // Convert a Map (from JSON) to an AlcoholHistory object
  static AlcoholHistory fromMap(Map<String, dynamic> map) {
    return AlcoholHistory(
      everConsumed: map['everConsumed'],
      currentlyConsuming: map['currentlyConsuming'],
      type: map['type'],
      frequency: map['frequency'],
      quantity: map['quantity'],
      yearsConsuming: map['yearsConsuming'],
      quitYear: map['quitYear'],
    );
  }

  // Serialize a list of AlcoholHistory objects to JSON
  static String toJson(List<AlcoholHistory> histories) {
    return jsonEncode(histories.map((history) => history.toMap()).toList());
  }

  // Deserialize a JSON string to a list of AlcoholHistory objects
  static List<AlcoholHistory> fromJson(String jsonString) {
    final List<dynamic> list = jsonDecode(jsonString);
    return list.map((map) => AlcoholHistory.fromMap(map)).toList();
  }

  // Save to local database
  static void saveToDb(AlcoholHistory data) {
    DbHandle db = DbHandle();
    db.setAlcoholHistory(db.getCurrentUserId(), jsonEncode(data.toMap()));
    db.close();
  }

  // Save to server
  static Future<void> saveToServer(AlcoholHistory data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveAlcoholHistory(
          db.getCurrentUserId()!, jsonEncode(data.toMap()));
    } catch (error) {
      print('Error saving alcohol history to server: $error');
      rethrow;
    } finally {
      api.close();
      db.close();
    }
  }
}

class AlcoholForm extends StatefulWidget {
  const AlcoholForm({super.key});

  @override
  State<AlcoholForm> createState() => _AlcoholFormState();
}

class _AlcoholFormState extends State<AlcoholForm> {
  final _formKeys = List.generate(3, (_) => GlobalKey<FormState>());
  final PageController _pageController = PageController();
  int _currentPage = 0;

  // Alcohol Controllers
  bool _everConsumedAlcohol = false;
  bool _currentlyConsumingAlcohol = false;
  final TextEditingController _alcoholTypeController = TextEditingController();
  final TextEditingController _alcoholFrequencyController =
      TextEditingController();
  final TextEditingController _alcoholQuantityController =
      TextEditingController();
  final TextEditingController _alcoholYearsConsumingController =
      TextEditingController();
  final TextEditingController _alcoholQuitYearController =
      TextEditingController();

  @override
  void dispose() {
    _pageController.dispose();
    // Dispose all controllers
    _alcoholTypeController.dispose();
    _alcoholFrequencyController.dispose();
    _alcoholQuantityController.dispose();
    _alcoholYearsConsumingController.dispose();
    _alcoholQuitYearController.dispose();
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

  void _navigateToSmokingForm(BuildContext context) {
    Navigator.push(
      context,
      MaterialPageRoute(
        builder: (context) =>
            SmokingForm(), // Replace with your next form
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Alcohol History'),
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
                _buildAlcoholStatusForm(),
                _buildAlcoholDetailsForm(),
                if (_everConsumedAlcohol && !_currentlyConsumingAlcohol)
                  _buildAlcoholQuitForm(),
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
                        if (_everConsumedAlcohol) {
                          _goToNextPage();
                        } else {
                          _saveAndNavigate(context);
                        }
                      } else if (_currentPage == 1) {
                        if (_currentlyConsumingAlcohol) {
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

  Widget _buildAlcoholStatusForm() {
    return Form(
      key: _formKeys[0],
      child: SingleChildScrollView(
        child: Column(
          children: [
            const Padding(
              padding: EdgeInsets.all(8.0),
              child: Text('Alcohol Consumption',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
            ),
            SwitchListTile(
              title: const Text('Have you ever consumed alcohol?'),
              value: _everConsumedAlcohol,
              onChanged: (value) {
                setState(() {
                  _everConsumedAlcohol = value;
                  if (!value) {
                    _currentlyConsumingAlcohol = false;
                  }
                });
              },
            ),
            if (_everConsumedAlcohol)
              SwitchListTile(
                title: const Text('Currently Consuming Alcohol?'),
                value: _currentlyConsumingAlcohol,
                onChanged: (value) {
                  setState(() {
                    _currentlyConsumingAlcohol = value;
                  });
                },
              ),
          ],
        ),
      ),
    );
  }

  Widget _buildAlcoholDetailsForm() {
    return Form(
      key: _formKeys[1],
      child: SingleChildScrollView(
        child: Column(
          children: [
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: DropdownButtonFormField<String>(
                decoration: const InputDecoration(labelText: 'Type of Alcohol'),
                value: _alcoholTypeController.text.isNotEmpty
                    ? _alcoholTypeController.text
                    : null,
                items: ['Beer', 'Wine', 'Spirits']
                    .map((type) => DropdownMenuItem(
                          value: type,
                          child: Text(type),
                        ))
                    .toList(),
                onChanged: (value) =>
                    setState(() => _alcoholTypeController.text = value!),
                validator: (value) =>
                    value == null || value.isEmpty ? 'Please select type' : null,
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: DropdownButtonFormField<String>(
                decoration: const InputDecoration(labelText: 'Frequency'),
                value: _alcoholFrequencyController.text.isNotEmpty
                    ? _alcoholFrequencyController.text
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
                    setState(() => _alcoholFrequencyController.text = value!),
                validator: (value) => value == null || value.isEmpty
                    ? 'Please select frequency'
                    : null,
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _alcoholQuantityController,
                decoration: const InputDecoration(labelText: 'Quantity'),
                keyboardType: TextInputType.number,
                validator: (value) => value == null || value.isEmpty
                    ? 'Please enter quantity'
                    : null,
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _alcoholYearsConsumingController,
                decoration: const InputDecoration(labelText: 'Years Consuming'),
                keyboardType: TextInputType.number,
                validator: (value) => value == null || value.isEmpty
                    ? 'Please enter years consuming'
                    : null,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildAlcoholQuitForm() {
    return Form(
      key: _formKeys[2],
      child: SingleChildScrollView(
        child: Column(
          children: [
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _alcoholQuitYearController,
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
    AlcoholHistory alcoholHistory = AlcoholHistory(
      everConsumed: _everConsumedAlcohol,
      currentlyConsuming: _currentlyConsumingAlcohol,
      type: _alcoholTypeController.text,
      frequency: _alcoholFrequencyController.text,
      quantity: int.tryParse(_alcoholQuantityController.text),
      yearsConsuming: int.tryParse(_alcoholYearsConsumingController.text),
      quitYear: int.tryParse(_alcoholQuitYearController.text),
    );

    AlcoholHistory.saveToDb(alcoholHistory);
    AlcoholHistory.saveToServer(alcoholHistory).then((_) {
      if (!context.mounted) return;
      print('Alcohol history data saved successfully');
      _navigateToSmokingForm(context);
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
