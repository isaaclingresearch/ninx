part of '../main.dart';

class FamilyChronicDiseaseData {
  final String name;
  final String details;
  final String familyMember;

  FamilyChronicDiseaseData({
    required this.name,
    required this.details,
    required this.familyMember,
  });

  Map<String, dynamic> toMap() {
    return {
      'name': name,
      'details': details,
      'relationship-with-family-member': familyMember,
    };
  }

  static void saveToDb(List<FamilyChronicDiseaseData> data) {
    DbHandle db = DbHandle();
    db.setFamilyIllnessHistory(
        db.getCurrentUserId(), jsonEncode(data.map((e) => e.toMap()).toList()));
    db.close();
  }

  static Future<void> saveToServer(List<FamilyChronicDiseaseData> data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.saveFamilyChronicDisease(db.getCurrentUserId()!,
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

class FamilyChronicDiseaseForm extends StatefulWidget {
  const FamilyChronicDiseaseForm({super.key});

  @override
  State<FamilyChronicDiseaseForm> createState() =>
      _FamilyChronicDiseaseFormState();
}

class _FamilyChronicDiseaseFormState extends State<FamilyChronicDiseaseForm> {
  final GlobalKey<FormState> _key = GlobalKey<FormState>();
  final GlobalKey<FormState> _detailsKey = GlobalKey<FormState>();

  int _currentIndex = 0;
  final PageController _pageController = PageController(initialPage: 0);

  // Common chronic diseases
  final List<String> _commonDiseases = [
    'Hypertension',
    'Diabetes',
    'Cancer',
    'Thyroid Disease',
    'Obesity',
    'Asthma',
    'Heart Disease',
    'Arthritis',
    'Depression',
    'Alzheimer\'s Disease'
  ];
  List<String> _selectedDiseases = [];

  // Controllers for details and family member
  List<List<TextEditingController>> _detailsControllers = [];

  @override
  void initState() {
    super.initState();
  }

  @override
  void dispose() {
    _pageController.dispose();
    for (var group in _detailsControllers) {
      for (var controller in group) {
        controller.dispose();
      }
    }
    super.dispose();
  }

  // Function to add text editing controllers dynamically
  void _addControllersForSelectedDiseases() {
    _detailsControllers = List.generate(
      _selectedDiseases.length,
      (_) => List.generate(2, (_) => TextEditingController()),
    );
  }

  List<Widget> _buildDiseaseDetailFields() {
    return [
      for (int i = 0; i < _selectedDiseases.length; i++) ...[
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: Text(
            _selectedDiseases[i],
            style: const TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][0],
            decoration: const InputDecoration(labelText: 'Details'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter details.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][1],
            decoration:
                const InputDecoration(labelText: 'Relationship with family member who had the disease?'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter ypur relationship with the family member.';
              }
              return null;
            },
          ),
        ),
      ],
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
          builder: (context) => FamilyChronicDiseaseForm(),
        ));
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Family Chronic Disease History'),
      ),
      body: Column(
        children: [
          Expanded(
            child: PageView(
              controller: _pageController,
              physics: const NeverScrollableScrollPhysics(),
              onPageChanged: (index) {
                setState(() {
                  _currentIndex = index;
                });
              },
              children: [
                // First Page: Multi-choice selection of common diseases
                Form(
                  key: _key,
                  child: ListView.builder(
                    itemCount: _commonDiseases.length,
                    itemBuilder: (context, index) {
                      final disease = _commonDiseases[index];
                      return CheckboxListTile(
                        title: Text(disease),
                        value: _selectedDiseases.contains(disease),
                        onChanged: (bool? value) {
                          setState(() {
                            if (value == true) {
                              _selectedDiseases.add(disease);
                            } else {
                              _selectedDiseases.remove(disease);
                            }
                          });
                        },
                      );
                    },
                  ),
                ),
                // Second Page: Details for each selected disease
                Form(
                  key: _detailsKey,
                  child: SingleChildScrollView(
                    child: Column(
                      children: _buildDiseaseDetailFields(),
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
                  onPressed: _currentIndex > 0 ? _goToPreviousPage : null,
                  child: const Icon(Icons.arrow_back),
                ),
                ElevatedButton(
                  onPressed: () async {
                    if (_currentIndex == 0) {
                      if (_key.currentState!.validate() &&
                          _selectedDiseases.isNotEmpty) {
                        setState(() {
                          _addControllersForSelectedDiseases();
                        });
                        _goToNextPage();
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                            content: Text(
                                'Please select at least one chronic disease.'),
                          ),
                        );
                      }
                    } else if (_currentIndex == 1) {
                      if (_detailsKey.currentState!.validate()) {
                        List<FamilyChronicDiseaseData> data = [
                          for (int i = 0; i < _selectedDiseases.length; i++)
                            FamilyChronicDiseaseData(
                              name: _selectedDiseases[i],
                              details: _detailsControllers[i][0].text,
                              familyMember: _detailsControllers[i][1].text,
                            ),
                        ];
                        FamilyChronicDiseaseData.saveToDb(data);
                        try {
                          await FamilyChronicDiseaseData.saveToServer(data);
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
                            content: Text(
                                'Please fill out all fields on this page.'),
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
