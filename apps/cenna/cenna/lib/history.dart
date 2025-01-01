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

  @override
  void initState() {
    super.initState();
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
        });
      }
    }
  }

  @override
  void dispose() {
    _countController.dispose();
    _pageController.dispose();
    for (var group in _detailsControllers) {
      for (var controller in group) {
        controller.dispose();
      }
    }
    super.dispose();
  }

  List<Widget> _makeDetailChildren() {
    return [
      for (int i = 0; i < _allergyCount; i++) ...[
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][0],
            decoration: const InputDecoration(labelText: 'Allergy Name'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the allergy name.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][1],
            decoration: const InputDecoration(labelText: 'Severity'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the severity.';
              }
              return null;
            },
          ),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8.0),
          child: TextFormField(
            controller: _detailsControllers[i][2],
            decoration: const InputDecoration(labelText: 'Reaction'),
            validator: (value) {
              if (value == null || value.isEmpty) {
                return 'Please enter the reaction.';
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

  void _handleNextButtonPressed(BuildContext context) {}

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
                    children: _makeDetailChildren(),
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
                      // Handle validation for the second page if needed
                      if (_detailsKey.currentState!.validate()) {
                        // Navigate to the next screen or perform other actions
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
