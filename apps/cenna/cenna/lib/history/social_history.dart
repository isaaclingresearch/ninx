part of '../main.dart';

class SocialHistoryData {
  final AlcoholHistory? alcoholHistory;
  final SmokingHistory? smokingHistory;
  final IllicitDrugHistory? illicitDrugHistory;
  final String? physicalActivity; // Could be an enum or a free text description
  final String? religion;
  final bool? hasFriends;
  final bool? hasCloseFamilyTies;

  SocialHistoryData({
    this.alcoholHistory,
    this.smokingHistory,
    this.illicitDrugHistory,
    this.physicalActivity,
    this.religion,
    this.hasFriends,
    this.hasCloseFamilyTies,
  });

  Map<String, dynamic> toMap() {
    return {
      'alcohol-history': alcoholHistory?.toMap(),
      'smoking-history': smokingHistory?.toMap(),
      'illicit-drug-history': illicitDrugHistory?.toMap(),
      'physical-activity': physicalActivity,
      'religion': religion,
      'has-friends': hasFriends,
      'has-close-family-ties': hasCloseFamilyTies,
    };
  }

  static void saveToDb(SocialHistoryData data) {
    DbHandle db = DbHandle();
    db.setSocialHistory(db.getCurrentUserId(), jsonEncode(data.toMap()));
    db.close();
  }

  static Future<void> saveToServer(SocialHistoryData data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveSocialHistory(
          db.getCurrentUserId()!, jsonEncode(data.toMap()));
    } catch (error) {
      print('Error saving social history to server: $error');
      rethrow; // Re-throw the error to propagate it further
    } finally {
      api.close();
      db.close();
    }
  }
}

class AlcoholHistory {
  final bool currentlyConsuming;
  final String? type; // e.g., Beer, Wine, Spirits
  final String? frequency; // e.g., Daily, Weekly, Monthly, Socially
  final int? quantity; // e.g., number of drinks per occasion, or units per week
  final int? yearsConsuming;

  AlcoholHistory({
    required this.currentlyConsuming,
    this.type,
    this.frequency,
    this.quantity,
    this.yearsConsuming,
  });

  Map<String, dynamic> toMap() {
    return {
      'currentlyConsuming': currentlyConsuming,
      'type': type,
      'frequency': frequency,
      'quantity': quantity,
      'yearsConsuming': yearsConsuming
    };
  }
}

class SmokingHistory {
  final bool currentlySmoking;
  final String? type; // e.g., Cigarettes, Cigars, Pipe, E-cigarettes
  final int? quantity; // e.g., packs per day, cigarettes per day
  final int? yearsSmoking;
  final int? quitYear; // Year they quit (if applicable)

  SmokingHistory({
    required this.currentlySmoking,
    this.type,
    this.quantity,
    this.yearsSmoking,
    this.quitYear,
  });

  Map<String, dynamic> toMap() {
    return {
      'currentlySmoking': currentlySmoking,
      'type': type,
      'quantity': quantity,
      'yearsSmoking': yearsSmoking,
      'quitYear': quitYear,
    };
  }
}

class IllicitDrugHistory {
  final bool currentlyUsing;
  final String?
      type; // e.g., Marijuana, Cocaine, Heroin, etc. (could be a multi-select)
  final String? frequency; // e.g., Daily, Weekly, Monthly
  final int? yearsUsing;
  final int? quitYear; // Year they quit (if applicable)

  IllicitDrugHistory({
    required this.currentlyUsing,
    this.type,
    this.frequency,
    this.yearsUsing,
    this.quitYear,
  });

  Map<String, dynamic> toMap() {
    return {
      'currentlyUsing': currentlyUsing,
      'type': type,
      'frequency': frequency,
      'yearsUsing': yearsUsing,
      'quitYear': quitYear,
    };
  }
}

class SocialHistoryForm extends StatefulWidget {
  const SocialHistoryForm({super.key});

  @override
  State<SocialHistoryForm> createState() => _SocialHistoryFormState();
}

class _SocialHistoryFormState extends State<SocialHistoryForm> {
  final _formKeys = List.generate(6, (_) => GlobalKey<FormState>());
  final PageController _pageController = PageController();
  int _currentPage = 0;

  // Alcohol Controllers
  bool _currentlyConsumingAlcohol = false;
  final TextEditingController _alcoholTypeController = TextEditingController();
  final TextEditingController _alcoholFrequencyController = TextEditingController();
  final TextEditingController _alcoholQuantityController = TextEditingController();
  final TextEditingController _alcoholYearsConsumingController =
      TextEditingController();

  // Smoking Controllers
  bool _currentlySmoking = false;
  final TextEditingController _smokingTypeController = TextEditingController();
  final TextEditingController _smokingQuantityController = TextEditingController();
  final TextEditingController _smokingYearsController = TextEditingController();
  final TextEditingController _smokingQuitYearController = TextEditingController();

  // Illicit Drug Controllers
  bool _currentlyUsingDrugs = false;
  final TextEditingController _drugTypeController = TextEditingController();
  final TextEditingController _drugFrequencyController = TextEditingController();
  final TextEditingController _drugYearsUsingController = TextEditingController();
  final TextEditingController _drugQuitYearController = TextEditingController();

  // Other Controllers
  final TextEditingController _physicalActivityController = TextEditingController();
  final TextEditingController _religionController = TextEditingController();
  bool? _hasFriends;
  bool? _hasCloseFamilyTies;

  @override
  void dispose() {
    _pageController.dispose();
    // Dispose all controllers
    _alcoholTypeController.dispose();
    _alcoholFrequencyController.dispose();
    _alcoholQuantityController.dispose();
    _alcoholYearsConsumingController.dispose();
    _smokingTypeController.dispose();
    _smokingQuantityController.dispose();
    _smokingYearsController.dispose();
    _smokingQuitYearController.dispose();
    _drugTypeController.dispose();
    _drugFrequencyController.dispose();
    _drugYearsUsingController.dispose();
    _drugQuitYearController.dispose();
    _physicalActivityController.dispose();
    _religionController.dispose();
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

  void _navigateToAdmissionForm(BuildContext context) {
    Navigator.push(
      context,
      MaterialPageRoute(
        builder: (context) =>
            SocialHistoryForm(), // Replace with your next form
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Social History'),
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
                _buildAlcoholForm(),
                _buildSmokingForm(),
                _buildIllicitDrugForm(),
                _buildPhysicalActivityForm(),
                _buildReligionForm(),
                _buildSocialTiesForm(),
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
                  onPressed: () {
                    if (_formKeys[_currentPage].currentState!.validate()) {
                      if (_currentPage < 5) {
                        _goToNextPage();
                      } else {
                        _submitForm(context);
                      }
                    } else {
                      ScaffoldMessenger.of(context).showSnackBar(
                        const SnackBar(
                          content: Text('Please fill out all required fields.'),
                        ),
                      );
                    }
                  },
                  child: _currentPage < 5
                      ? const Icon(Icons.arrow_forward)
                      : const Text("Submit"),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildAlcoholForm() {
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
              title: const Text('Currently Consuming Alcohol?'),
              value: _currentlyConsumingAlcohol,
              onChanged: (value) {
                setState(() {
                  _currentlyConsumingAlcohol = value;
                });
              },
            ),
            if (_currentlyConsumingAlcohol) ...[
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _alcoholTypeController,
                  decoration:
                      const InputDecoration(labelText: 'Type of Alcohol'),
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please enter type'
                      : null,
                ),
              ),
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _alcoholFrequencyController,
                  decoration: const InputDecoration(labelText: 'Frequency'),
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please enter frequency'
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
                  decoration:
                      const InputDecoration(labelText: 'Years Consuming'),
                  keyboardType: TextInputType.number,
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please enter years consuming'
                      : null,
                ),
              ),
            ],
          ],
        ),
      ),
    );
  }

  Widget _buildSmokingForm() {
    return Form(
      key: _formKeys[1],
      child: SingleChildScrollView(
        child: Column(
          children: [
            const Padding(
              padding: EdgeInsets.all(8.0),
              child: Text('Smoking History',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
            ),
            SwitchListTile(
              title: const Text('Currently Smoking?'),
              value: _currentlySmoking,
              onChanged: (value) {
                setState(() {
                  _currentlySmoking = value;
                });
              },
            ),
            if (_currentlySmoking) ...[
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _smokingTypeController,
                  decoration:
                      const InputDecoration(labelText: 'Type of Smoking'),
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please enter type'
                      : null,
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
            ] else ...[
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _smokingQuitYearController,
                  decoration: const InputDecoration(
                      labelText: 'Year Quit (if applicable)'),
                  keyboardType: TextInputType.number,
                  validator: (value) {
                    if (!_currentlySmoking &&
                        (value == null || value.isEmpty)) {
                      return 'Please enter the year you quit';
                    }
                    return null;
                  },
                ),
              ),
            ],
          ],
        ),
      ),
    );
  }

  Widget _buildIllicitDrugForm() {
    return Form(
      key: _formKeys[2],
      child: SingleChildScrollView(
        child: Column(
          children: [
            const Padding(
              padding: EdgeInsets.all(8.0),
              child: Text('Illicit Drug Use',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
            ),
            SwitchListTile(
              title: const Text('Currently Using Illicit Drugs?'),
              value: _currentlyUsingDrugs,
              onChanged: (value) {
                setState(() {
                  _currentlyUsingDrugs = value;
                });
              },
            ),
            if (_currentlyUsingDrugs) ...[
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _drugTypeController,
                  decoration:
                      const InputDecoration(labelText: 'Type of Drug(s)'),
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please enter type'
                      : null,
                ),
              ),
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _drugFrequencyController,
                  decoration: const InputDecoration(labelText: 'Frequency'),
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please enter frequency'
                      : null,
                ),
              ),
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _drugYearsUsingController,
                  decoration: const InputDecoration(labelText: 'Years Using'),
                  keyboardType: TextInputType.number,
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please enter years using'
                      : null,
                ),
              ),
            ] else ...[
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _drugQuitYearController,
                  decoration: const InputDecoration(
                      labelText: 'Year Quit (if applicable)'),
                  keyboardType: TextInputType.number,
                  validator: (value) {
                    if (!_currentlyUsingDrugs &&
                        (value == null || value.isEmpty)) {
                      return 'Please enter the year you quit';
                    }
                    return null;
                  },
                ),
              ),
            ],
          ],
        ),
      ),
    );
  }

  Widget _buildPhysicalActivityForm() {
    return Form(
      key: _formKeys[3],
      child: SingleChildScrollView(
        child: Column(
          children: [
            const Padding(
              padding: EdgeInsets.all(8.0),
              child: Text('Physical Activity',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _physicalActivityController,
                decoration: const InputDecoration(
                    labelText: 'Describe your physical activity level'),
                minLines: 2,
                maxLines: 5,
                validator: (value) => value == null || value.isEmpty
                    ? 'Please enter your physical activity'
                    : null,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildReligionForm() {
    return Form(
      key: _formKeys[4],
      child: SingleChildScrollView(
        child: Column(
          children: [
            const Padding(
              padding: EdgeInsets.all(8.0),
              child: Text('Religion',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: TextFormField(
                controller: _religionController,
                decoration: const InputDecoration(
                    labelText: 'Enter your religion (if any)'),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildSocialTiesForm() {
    return Form(
      key: _formKeys[5],
      child: SingleChildScrollView(
        child: Column(
          children: [
            const Padding(
              padding: EdgeInsets.all(8.0),
              child: Text('Social Ties',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
            ),
            SwitchListTile(
              title: const Text('Do you have friends?'),
              value: _hasFriends ?? false,
              onChanged: (value) {
                setState(() {
                  _hasFriends = value;
                });
              },
            ),
            SwitchListTile(
              title: const Text('Do you have close family ties?'),
              value: _hasCloseFamilyTies ?? false,
              onChanged: (value) {
                setState(() {
                  _hasCloseFamilyTies = value;
                });
              },
            ),
          ],
        ),
      ),
    );
  }

  void _submitForm(BuildContext context) {
    if (_formKeys.every((key) => key.currentState!.validate())) {
      // Create AlcoholHistory object
      AlcoholHistory? alcoholHistory = _currentlyConsumingAlcohol
          ? AlcoholHistory(
              currentlyConsuming: _currentlyConsumingAlcohol,
              type: _alcoholTypeController.text,
              frequency: _alcoholFrequencyController.text,
              quantity: int.tryParse(_alcoholQuantityController.text),
              yearsConsuming:
                  int.tryParse(_alcoholYearsConsumingController.text),
            )
          : null;

      // Create SmokingHistory object
      SmokingHistory? smokingHistory = SmokingHistory(
        currentlySmoking: _currentlySmoking,
        type: _smokingTypeController.text,
        quantity: int.tryParse(_smokingQuantityController.text),
        yearsSmoking: int.tryParse(_smokingYearsController.text),
        quitYear: int.tryParse(_smokingQuitYearController.text),
      );

      // Create IllicitDrugHistory object
      IllicitDrugHistory? illicitDrugHistory = _currentlyUsingDrugs
          ? IllicitDrugHistory(
              currentlyUsing: _currentlyUsingDrugs,
              type: _drugTypeController.text,
              frequency: _drugFrequencyController.text,
              yearsUsing: int.tryParse(_drugYearsUsingController.text),
              quitYear: int.tryParse(_drugQuitYearController.text),
            )
          : null;

      SocialHistoryData socialHistoryData = SocialHistoryData(
        alcoholHistory: alcoholHistory,
        smokingHistory: smokingHistory,
        illicitDrugHistory: illicitDrugHistory,
        physicalActivity: _physicalActivityController.text,
        religion: _religionController.text,
        hasFriends: _hasFriends,
        hasCloseFamilyTies: _hasCloseFamilyTies,
      );

      // Save data using the updated functions
      SocialHistoryData.saveToDb(socialHistoryData);
      SocialHistoryData.saveToServer(socialHistoryData)
          .then((_) {
        if (!context.mounted) return;
        print('Social history data saved successfully');
        _navigateToAdmissionForm(context);
      })
          .catchError((error) {
        if (!context.mounted) return;
        print('Error: $error');
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Failed to save data: $error'),
          ),
        );
      });
    } else {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('Please fill out all required fields.'),
        ),
      );
    }
  }
}
