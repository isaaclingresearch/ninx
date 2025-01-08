part of '../main.dart';

class PhysicalActivity {
  final List<String> types; // e.g., Walking, Running, Cycling, Swimming, etc.
  final String? frequency; // e.g., Daily, Weekly, Monthly
  final double? intensity; // e.g., Light, Moderate, Vigorous
  final int? yearsSpent; // Number of years engaged in physical activity

  PhysicalActivity({
    required this.types,
    this.frequency,
    this.intensity,
    this.yearsSpent,
  });

  Map<String, dynamic> toMap() {
    return {
      'types': types,
      'frequency': frequency,
      'intensity': intensity,
      'yearsSpent': yearsSpent,
    };
  }

  // Convert a Map (from JSON) to a PhysicalActivity object
  static PhysicalActivity fromMap(Map<String, dynamic> map) {
    return PhysicalActivity(
      types: List<String>.from(map['types']),
      frequency: map['frequency'],
      intensity: map['intensity'],
      yearsSpent: map['yearsSpent'],
    );
  }

  // Serialize a list of PhysicalActivity objects to JSON
  static String serializeList(List<PhysicalActivity> activities) {
    return jsonEncode(activities.map((activity) => activity.toMap()).toList());
  }

  // Deserialize a JSON string to a list of PhysicalActivity objects
  static List<PhysicalActivity> deserializeList(String jsonString) {
    final List<dynamic> list = jsonDecode(jsonString);
    return list.map((map) => PhysicalActivity.fromMap(map)).toList();
  }

  // Save to local database
  static void saveToDb(PhysicalActivity data) {
    DbHandle db = DbHandle();
    db.setPhysicalActivity(db.getCurrentUserId(), jsonEncode(data.toMap()));
    db.close();
  }

  // Save to server
  static Future<void> saveToServer(PhysicalActivity data) async {
    Api api = Api();
    DbHandle db = DbHandle();
    try {
      await api.savePhysicalActivity(
          db.getCurrentUserId()!, jsonEncode(data.toMap()));
    } catch (error) {
      print('Error saving physical activity to server: $error');
      rethrow;
    } finally {
      api.close();
      db.close();
    }
  }
}

class PhysicalActivityForm extends StatefulWidget {
  const PhysicalActivityForm({super.key});

  @override
  State<PhysicalActivityForm> createState() => _PhysicalActivityFormState();
}

class _PhysicalActivityFormState extends State<PhysicalActivityForm> {
  final _formKey = GlobalKey<FormState>();

  // Physical Activity Controllers
  List<String> _selectedExerciseTypes = [];
  final TextEditingController _exerciseFrequencyController =
      TextEditingController();
  final TextEditingController _exerciseIntensityController =
      TextEditingController();
  final TextEditingController _exerciseYearsSpentController =
      TextEditingController();

  // Available exercise types
  final List<String> _exerciseTypes = [
    'Walking',
    'Running',
    'Cycling',
    'Swimming',
    'Yoga',
    'Weight Lifting',
    'Aerobics',
    'Other'
  ];

  @override
  void dispose() {
    _exerciseFrequencyController.dispose();
    _exerciseIntensityController.dispose();
    _exerciseYearsSpentController.dispose();
    super.dispose();
  }

  void _navigateToNextForm(BuildContext context) {
    Navigator.push(
      context,
      MaterialPageRoute(
        builder: (context) =>
            ReligionForm(), // Replace with your next form
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Physical Activity'),
      ),
      body: Form(
        key: _formKey,
        child: SingleChildScrollView(
          child: Column(
            children: [
              const Padding(
                padding: EdgeInsets.all(8.0),
                child: Text('Physical Activity',
                    style:
                        TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
              ),
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    const Text('Types of Exercise (Select Multiple)'),
                    ..._exerciseTypes.map((type) {
                      return CheckboxListTile(
                        title: Text(type),
                        value: _selectedExerciseTypes.contains(type),
                        onChanged: (bool? value) {
                          setState(() {
                            if (value != null) {
                              if (value) {
                                _selectedExerciseTypes.add(type);
                              } else {
                                _selectedExerciseTypes.remove(type);
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
                  value: _exerciseFrequencyController.text.isNotEmpty
                      ? _exerciseFrequencyController.text
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
                      setState(() => _exerciseFrequencyController.text = value!),
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please select frequency'
                      : null,
                ),
              ),
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: DropdownButtonFormField<String>(
                  decoration: const InputDecoration(labelText: 'Intensity'),
                  value: _exerciseIntensityController.text.isNotEmpty
                      ? _exerciseIntensityController.text
                      : null,
                  items: ['Light', 'Moderate', 'Vigorous']
                      .map((intensity) => DropdownMenuItem(
                            value: intensity,
                            child: Text(intensity),
                          ))
                      .toList(),
                  onChanged: (value) =>
                      setState(() => _exerciseIntensityController.text = value!),
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please select intensity'
                      : null,
                ),
              ),
              Padding(
                padding: const EdgeInsets.all(8.0),
                child: TextFormField(
                  controller: _exerciseYearsSpentController,
                  decoration:
                      const InputDecoration(labelText: 'Years Spent'),
                  keyboardType: TextInputType.number,
                  validator: (value) => value == null || value.isEmpty
                      ? 'Please enter years spent'
                      : null,
                ),
              ),
              Padding(
                padding: const EdgeInsets.all(16.0),
                child: ElevatedButton(
                  onPressed: () async {
                    if (_formKey.currentState!.validate()) {
                      _saveAndNavigate(context);
                    } else {
                      ScaffoldMessenger.of(context).showSnackBar(
                        const SnackBar(
                          content:
                              Text('Please fill out all required fields.'),
                        ),
                      );
                    }
                  },
                  child: const Text('Submit'),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  void _saveAndNavigate(BuildContext context) {
    PhysicalActivity physicalActivity = PhysicalActivity(
      types: _selectedExerciseTypes,
      frequency: _exerciseFrequencyController.text,
      intensity: double.tryParse(_exerciseIntensityController.text),
      yearsSpent: int.tryParse(_exerciseYearsSpentController.text),
    );

    PhysicalActivity.saveToDb(physicalActivity);
    PhysicalActivity.saveToServer(physicalActivity).then((_) {
      if (!context.mounted) return;
      print('Physical activity data saved successfully');
      _navigateToNextForm(context);
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
