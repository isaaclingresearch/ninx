part of '../main.dart';

class SmokingHistory {
  final bool everSmoked;
  final bool currentlySmoking;
  final String? type; // e.g., Cigarettes, Cigars, Pipe, E-cigarettes
  final int? quantity; // e.g., packs per day, cigarettes per day
  final int? yearsSmoking;
  final int? quitYear; // Year they quit (if applicable)

  SmokingHistory({
    required this.everSmoked,
    required this.currentlySmoking,
    this.type,
    this.quantity,
    this.yearsSmoking,
    this.quitYear,
  });

  Map<String, dynamic> toMap() {
    return {
      'everSmoked': everSmoked,
      'currentlySmoking': currentlySmoking,
      'type': type,
      'quantity': quantity,
      'yearsSmoking': yearsSmoking,
      'quitYear': quitYear,
    };
  }

  // Convert a Map (from JSON) to a SmokingHistory object
  static SmokingHistory fromMap(Map<String, dynamic> map) {
    return SmokingHistory(
      everSmoked: map['everSmoked'],
      currentlySmoking: map['currentlySmoking'],
      type: map['type'],
      quantity: map['quantity'],
      yearsSmoking: map['yearsSmoking'],
      quitYear: map['quitYear'],
    );
  }

  // Serialize a list of SmokingHistory objects to JSON
  static String serializeList(List<SmokingHistory> histories) {
    return jsonEncode(histories.map((history) => history.toMap()).toList());
  }

  // Deserialize a JSON string to a list of SmokingHistory objects
  static List<SmokingHistory> deserializeList(String jsonString) {
    final List<dynamic> list = jsonDecode(jsonString);
    return list.map((map) => SmokingHistory.fromMap(map)).toList();
  }

  // Save to local database
  static void saveToDb(SmokingHistory data) {
    DbHandle db = DbHandle();
    db.setSmokingHistory(db.getCurrentUserId(), jsonEncode(data.toMap()));
    db.close();
  }

  // Save to server
  static Future<void> saveToServer(SmokingHistory data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveSmokingHistory(
          db.getCurrentUserId()!, jsonEncode(data.toMap()));
    } catch (error) {
      print('Error saving smoking history to server: $error');
      rethrow;
    } finally {
      api.close();
      db.close();
    }
  }
}

class SmokingForm extends StatefulWidget {
  const SmokingForm({super.key});

  @override
  State<SmokingForm> createState() => _SmokingFormState();
}

class _SmokingFormState extends State<SmokingForm> {
  final _formKeys = List.generate(3, (_) => GlobalKey<FormState>());
  final PageController _pageController = PageController();
  int _currentPage = 0;

  // Smoking Controllers
  bool _everSmoked = false;
  bool _currentlySmoking = false;
  final TextEditingController _smokingTypeController = TextEditingController();
  final TextEditingController _smokingQuantityController =
      TextEditingController();
  final TextEditingController _smokingYearsController = TextEditingController();
  final TextEditingController _smokingQuitYearController =
      TextEditingController();

  @override
  void dispose() {
    _pageController.dispose();
    _smokingTypeController.dispose();
    _smokingQuantityController.dispose();
    _smokingYearsController.dispose();
    _smokingQuitYearController.dispose();
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

  void _navigateToIllicitDrugForm(BuildContext context) {
    Navigator.push(
      context,
      MaterialPageRoute(
        builder: (context) =>
            IllicitDrugForm(), // Replace with your next form
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Smoking History'),
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
                _buildSmokingStatusForm(),
                _buildSmokingDetailsForm(),
                if (_everSmoked && !_currentlySmoking)
                  _buildSmokingQuitForm(),
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
                        if (_everSmoked) {
                          _goToNextPage();
                        } else {
                          _saveAndNavigate(context);
                        }
                      } else if (_currentPage == 1) {
                        if (_currentlySmoking) {
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

  Widget _buildSmokingStatusForm() {
    return Form(
      key: _formKeys[0],
      child: SingleChildScrollView(
        child: Column(
          children: [
            const Padding(
              padding: EdgeInsets.all(8.0),
              child: Text('Smoking History',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
            ),
            SwitchListTile(
              title: const Text('Have you ever smoked?'),
              value: _everSmoked,
              onChanged: (value) {
                setState(() {
                  _everSmoked = value;
                  if (!value) {
                    _currentlySmoking = false;
                  }
                });
              },
            ),
            if (_everSmoked)
              SwitchListTile(
                title: const Text('Currently Smoking?'),
                value: _currentlySmoking,
                onChanged: (value) {
                  setState(() {
                    _currentlySmoking = value;
                  });
                },
              ),
          ],
        ),
      ),
    );
  }

  Widget _buildSmokingDetailsForm() {
    return Form(
      key: _formKeys[1],
      child: SingleChildScrollView(
        child: Column(
          children: [
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _smokingTypeController,
                decoration: const InputDecoration(labelText: 'Type of Smoking'),
                validator: (value) =>
                    value == null || value.isEmpty ? 'Please enter type' : null,
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _smokingQuantityController,
                decoration: const InputDecoration(
                    labelText: 'Quantity (e.g., packs per day)'),
                keyboardType: TextInputType.number,
                validator: (value) => value == null || value.isEmpty
                    ? 'Please enter quantity'
                    : null,
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _smokingYearsController,
                decoration: const InputDecoration(labelText: 'Years Smoking'),
                keyboardType: TextInputType.number,
                validator: (value) => value == null || value.isEmpty
                    ? 'Please enter years smoking'
                    : null,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildSmokingQuitForm() {
    return Form(
      key: _formKeys[2],
      child: SingleChildScrollView(
        child: Column(
          children: [
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _smokingQuitYearController,
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
    SmokingHistory smokingHistory = SmokingHistory(
      everSmoked: _everSmoked,
      currentlySmoking: _currentlySmoking,
      type: _smokingTypeController.text,
      quantity: int.tryParse(_smokingQuantityController.text),
      yearsSmoking: int.tryParse(_smokingYearsController.text),
      quitYear: int.tryParse(_smokingQuitYearController.text),
    );

    SmokingHistory.saveToDb(smokingHistory);
    SmokingHistory.saveToServer(smokingHistory).then((_) {
      if (!context.mounted) return;
      print('Smoking history data saved successfully');
      _navigateToIllicitDrugForm(context);
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
