import 'package:flutter/material.dart';

void main() {
  runApp(const Cenna());
}

class Cenna extends StatelessWidget {
  const Cenna({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Cenna',
      home: const DemographicsForm(),
    );
  }
}

class DemographicsForm extends StatefulWidget {
  const DemographicsForm({super.key});
  @override
  State<DemographicsForm> createState() => _DemographicsFormState();
}

class _DemographicsFormState extends State<DemographicsForm> {
  int currentIndex = 0;
  final PageController _pageController = PageController(initialPage: 0);
  final int _numPages = 4; // Number of pages
  final List<GlobalKey<FormState>> _formKeys = List.generate(4, (_) => GlobalKey<FormState>());

  // Controllers for Page 1
  final TextEditingController _page1Controller1 = TextEditingController();
  final TextEditingController _page1Controller2 = TextEditingController();

  // Controllers for Page 2
  final TextEditingController _page2Controller1 = TextEditingController();
  final TextEditingController _page2Controller2 = TextEditingController();

  // Controllers for Page 3
  final TextEditingController _page3Controller1 = TextEditingController();
  final TextEditingController _page3Controller2 = TextEditingController();

  // Controllers for Page 4
  final TextEditingController _page4Controller1 = TextEditingController();
  final TextEditingController _page4Controller2 = TextEditingController();

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

  void _submitForm() {
    bool allValid = true;
    for (final key in _formKeys) {
      if (key.currentState != null && !key.currentState!.validate()) {
        allValid = false;
      }
    }

    if (allValid) {
      // Access the data from all text fields
      final formData = [
        [_page1Controller1.text, _page1Controller2.text],
        [_page2Controller1.text, _page2Controller2.text],
        [_page3Controller1.text, _page3Controller2.text],
        [_page4Controller1.text, _page4Controller2.text],
      ];
      print('Submitted Form Data: $formData');
      // You can perform further actions here, like sending data to an API
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Processing Data')),
      );
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Demographics Form')),
      body: Column(
        children: [
          const Text('Demographics'),
          LinearProgressIndicator(
            color: const Color(0xFF1e90ff),
            value: currentIndex / (_numPages - 1),
            borderRadius: BorderRadius.circular(10),
            minHeight: 10.0,
          ),
          Expanded(
            child: PageView(
              controller: _pageController,
              physics: const NeverScrollableScrollPhysics(),
              onPageChanged: (i) => setState(() {
                currentIndex = i;
              }),
              children: [
                // Page 1
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Form(
                    key: _formKeys[0],
                    child: Column(
                      children: [
                        const Text('Page 1 Entries'),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page1Controller1,
                            decoration: const InputDecoration(labelText: 'Entry 1'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page1Controller2,
                            decoration: const InputDecoration(labelText: 'Entry 2'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
                            },
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
                // Page 2
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Form(
                    key: _formKeys[1],
                    child: Column(
                      children: [
                        const Text('Page 2 Entries'),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page2Controller1,
                            decoration: const InputDecoration(labelText: 'Entry 1'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page2Controller2,
                            decoration: const InputDecoration(labelText: 'Entry 2'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
                            },
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
                // Page 3
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Form(
                    key: _formKeys[2],
                    child: Column(
                      children: [
                        const Text('Page 3 Entries'),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page3Controller1,
                            decoration: const InputDecoration(labelText: 'Entry 1'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page3Controller2,
                            decoration: const InputDecoration(labelText: 'Entry 2'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
                            },
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
                // Page 4
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Form(
                    key: _formKeys[3],
                    child: Column(
                      children: [
                        const Text('Page 4 Entries'),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page4Controller1,
                            decoration: const InputDecoration(labelText: 'Entry 1'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page4Controller2,
                            decoration: const InputDecoration(labelText: 'Entry 2'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter some text';
                              }
                              return null;
                            },
                          ),
                        ),
                      ],
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
                  onPressed: currentIndex > 0 ? _goToPreviousPage : null,
                  child: const Icon(Icons.arrow_back),
                ),
                ElevatedButton(
                  onPressed: () {
                    if (currentIndex < _numPages - 1) {
                      // Validate the current form before moving to the next page
                      if (_formKeys[currentIndex].currentState!.validate()) {
                        _goToNextPage();
                      } else {
                        // Provide visual feedback or a message if the form is invalid
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(content: Text('Please fill out all fields on this page')),
                        );
                      }
                    } else {
                      _submitForm();
                    }
                  },
                  child: Icon(currentIndex == _numPages - 1 ? Icons.check : Icons.arrow_forward),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  @override
  void dispose() {
    _pageController.dispose();
    _page1Controller1.dispose();
    _page1Controller2.dispose();
    _page2Controller1.dispose();
    _page2Controller2.dispose();
    _page3Controller1.dispose();
    _page3Controller2.dispose();
    _page4Controller1.dispose();
    _page4Controller2.dispose();
    super.dispose();
  }
}
