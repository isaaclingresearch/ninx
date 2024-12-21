import 'package:flutter/material.dart';
import 'package:intl/intl.dart'; // For date formatting

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
  final List<GlobalKey<FormState>> _formKeys =
      List.generate(4, (_) => GlobalKey<FormState>());

  // Controllers for Page 1
  final TextEditingController _fullNameController = TextEditingController();
  String? _selectedSex; // This will hold "Male" or "Female"
  String? _selectedGender;
  DateTime? _selectedDate; // This will hold the selected date

  // Controllers for Page 2
  final TextEditingController _page2Controller1 = TextEditingController();
  final TextEditingController _page2Controller2 = TextEditingController();

  // Controllers for Page 3
  final TextEditingController _page3Controller1 = TextEditingController();
  final TextEditingController _page3Controller2 = TextEditingController();

  // Controllers for Page 4
  final TextEditingController _page4Controller1 = TextEditingController();
  final TextEditingController _page4Controller2 = TextEditingController();

  Future<void> _selectDate(BuildContext context) async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate:
          _selectedDate ?? DateTime.now(), // Show today if no date selected
      firstDate: DateTime(1900), // Adjust range as needed
      lastDate: DateTime.now(),
    );

    if (pickedDate != null && pickedDate != _selectedDate) {
      setState(() {
        _selectedDate = pickedDate;
      });
    }
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

  void _navigateToForm1(BuildContext context) {
    bool demographicsValid = true;
    for (final key in _formKeys) {
      if (key.currentState != null && !key.currentState!.validate()) {
        demographicsValid = false;
      }
    }

    if (demographicsValid) {
      Navigator.push(
          context,
          MaterialPageRoute(
            builder: (context) => Form1(
              demographicsData: {
                'fullname': _fullNameController.text,
                'sex': _selectedSex,
                'gender': _selectedGender,
                'date-of-birth': _selectedDate != null
                    ? DateFormat('dd-MM-yyyy').format(_selectedDate!)
                    : '',
                'page21': _page2Controller1.text,
                'page22': _page2Controller2.text,
                'page31': _page3Controller1.text,
                'page3': _page3Controller2.text,
                'page41': _page4Controller1.text,
                'page42': _page4Controller2.text
              },
            ),
          ));
    } else {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
            content: Text('Please fill out all fields on this form')),
      );
    }
  }

  void _showWhyThisDialog(int pageIndex) {
    String title = '';
    String content = '';
    switch (pageIndex) {
      case 0:
        title = 'Why Identification Data?';
        content = 'We need to know who you are and how to address you. Some diseases are sex specific, some are age related.';
        break;
      case 1:
        title = 'Why Page 2 Data?';
        content =
            'The data on Page 2 helps us gather specific details relevant to your experience and needs.';
        break;
      case 2:
        title = 'Why Page 3 Data?';
        content =
            'Information from Page 3 is used for analysis and to tailor our services to your preferences.';
        break;
      case 3:
        title = 'Why Page 4 Data?';
        content =
            'Page 4 collects final details necessary for processing your request and ensuring accuracy.';
        break;
    }
    showDialog(
      context: context,
      builder: (BuildContext context) {
        return AlertDialog(
          title: Text(title),
          content: Text(content),
          actions: <Widget>[
            TextButton(
              child: const Text('Close'),
              onPressed: () {
                Navigator.of(context).pop();
              },
            ),
          ],
        );
      },
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Demographics')),
      body: Column(
        children: [
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
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            const Text('Identification'),
                            TextButton(
                              onPressed: () => _showWhyThisDialog(0),
                              child: const Text('Why this?'),
                            ),
                          ],
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _fullNameController,
                            decoration:
                                const InputDecoration(labelText: 'Full name'),
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please enter your full name.';
                              }
                              return null;
                            },
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: DropdownButtonFormField<String>(
                            decoration: const InputDecoration(
                              labelText: 'Sex assigned at birth',
                            ),
                            value: _selectedSex, // Currently selected value
                            onChanged: (String? newValue) {
                              setState(() {
                                _selectedSex = newValue;
                              });
                            },
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please select sex assigned at birth';
                              }
                              return null;
                            },
                            items: const <DropdownMenuItem<String>>[
                              DropdownMenuItem<String>(
                                value: 'Male',
                                child: Text('Male'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Female',
                                child: Text('Female'),
                              ),
                            ],
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: DropdownButtonFormField<String>(
                            decoration: const InputDecoration(
                              labelText: 'Gender',
                            ),
                            value: _selectedGender, // Currently selected value
                            onChanged: (String? newValue) {
                              setState(() {
                                _selectedGender = newValue;
                              });
                            },
                            validator: (value) {
                              if (value == null || value.isEmpty) {
                                return 'Please select sex assigned at birth';
                              }
                              return null;
                            },
                            items: const <DropdownMenuItem<String>>[
                              DropdownMenuItem<String>(
                                value: 'Male',
                                child: Text('Male'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Female',
                                child: Text('Female'),
                              ),
                              DropdownMenuItem<String>(
                                value: 'Other',
                                child: Text('Other'),
                              ),
                            ],
                          ),
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            readOnly: true, // Prevent manual editing
                            onTap: () =>
                                _selectDate(context), // Open date picker on tap
                            controller: TextEditingController(
                              text: _selectedDate != null
                                  ? DateFormat('dd-MM-yyyy')
                                      .format(_selectedDate!)
                                  : '', // Format and display date
                            ),
                            decoration: const InputDecoration(
                              labelText: 'Date of Birth',
                              suffixIcon: Icon(
                                  Icons.calendar_today), // Add a calendar icon
                            ),
                            validator: (value) {
                              if (_selectedDate == null) {
                                return 'Please select a date';
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
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            const Text('Page 2 Entries'),
                            TextButton(
                              onPressed: () => _showWhyThisDialog(1),
                              child: const Text('Why this?'),
                            ),
                          ],
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page2Controller1,
                            decoration:
                                const InputDecoration(labelText: 'Entry 1'),
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
                            decoration:
                                const InputDecoration(labelText: 'Entry 2'),
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
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            const Text('Page 3 Entries'),
                            TextButton(
                              onPressed: () => _showWhyThisDialog(2),
                              child: const Text('Why this?'),
                            ),
                          ],
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page3Controller1,
                            decoration:
                                const InputDecoration(labelText: 'Entry 1'),
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
                            decoration:
                                const InputDecoration(labelText: 'Entry 2'),
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
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            const Text('Page 4 Entries'),
                            TextButton(
                              onPressed: () => _showWhyThisDialog(3),
                              child: const Text('Why this?'),
                            ),
                          ],
                        ),
                        Padding(
                          padding: const EdgeInsets.symmetric(vertical: 8.0),
                          child: TextFormField(
                            controller: _page4Controller1,
                            decoration:
                                const InputDecoration(labelText: 'Entry 1'),
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
                            decoration:
                                const InputDecoration(labelText: 'Entry 2'),
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
                      if (_formKeys[currentIndex].currentState!.validate()) {
                        _goToNextPage();
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                              content: Text(
                                  'Please fill out all fields on this page')),
                        );
                      }
                    } else {
                      _navigateToForm1(context);
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

  @override
  void dispose() {
    _pageController.dispose();
    _fullNameController.dispose();
    _page2Controller1.dispose();
    _page2Controller2.dispose();
    _page3Controller1.dispose();
    _page3Controller2.dispose();
    _page4Controller1.dispose();
    _page4Controller2.dispose();
    super.dispose();
  }
}

class Form1 extends StatefulWidget {
  const Form1({super.key, this.demographicsData});

  final Map<String, dynamic>? demographicsData;

  @override
  State<Form1> createState() => _Form1State();
}

class _Form1State extends State<Form1> {
  final GlobalKey<FormState> _form1Key = GlobalKey<FormState>();
  final TextEditingController _form1Controller1 = TextEditingController();
  final TextEditingController _form1Controller2 = TextEditingController();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Form 1')),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _form1Key,
          child: Column(
            children: [
              const Text('Form 1 Entries'),
              Padding(
                padding: const EdgeInsets.symmetric(vertical: 8.0),
                child: TextFormField(
                  controller: _form1Controller1,
                  decoration:
                      const InputDecoration(labelText: 'Form 1 Entry 1'),
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
                  controller: _form1Controller2,
                  decoration:
                      const InputDecoration(labelText: 'Form 1 Entry 2'),
                  validator: (value) {
                    if (value == null || value.isEmpty) {
                      return 'Please enter some text';
                    }
                    return null;
                  },
                ),
              ),
              const SizedBox(height: 20),
            ],
          ),
        ),
      ),
      bottomNavigationBar: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            ElevatedButton(
              onPressed: () => Navigator.pop(context),
              child: const Icon(Icons.arrow_back),
            ),
            ElevatedButton(
              onPressed: () {
                if (_form1Key.currentState!.validate()) {
                  final form1Data = {
                    'entry1': _form1Controller1.text,
                    'entry2': _form1Controller2.text,
                  };
                  Navigator.push(
                    context,
                    MaterialPageRoute(
                      builder: (context) => Form2(
                        demographicsData: widget.demographicsData,
                        form1Data: form1Data,
                      ),
                    ),
                  );
                } else {
                  ScaffoldMessenger.of(context).showSnackBar(
                    const SnackBar(
                        content:
                            Text('Please fill out all fields on this form')),
                  );
                }
              },
              child: const Icon(Icons.arrow_forward),
            ),
          ],
        ),
      ),
    );
  }

  @override
  void dispose() {
    _form1Controller1.dispose();
    _form1Controller2.dispose();
    super.dispose();
  }
}

class Form2 extends StatefulWidget {
  const Form2({super.key, this.demographicsData, this.form1Data});

  final Map<String, dynamic>? demographicsData;
  final Map<String, String>? form1Data;

  @override
  State<Form2> createState() => _Form2State();
}

class _Form2State extends State<Form2> {
  final GlobalKey<FormState> _form2Key = GlobalKey<FormState>();
  final TextEditingController _form2Controller1 = TextEditingController();
  final TextEditingController _form2Controller2 = TextEditingController();

  void _submitAllForms(BuildContext context) {
    if (_form2Key.currentState!.validate()) {
      final form2Data = {
        'entry1': _form2Controller1.text,
        'entry2': _form2Controller2.text,
      };

      final combinedFormData = {
        'demographics': widget.demographicsData,
        'form1': widget.form1Data,
        'form2': form2Data,
      };
      print('Combined Form Data: $combinedFormData');

      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Processing All Data')),
      );
      // You might want to navigate to a success screen here
    } else {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
            content: Text('Please fill out all fields on this form')),
      );
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Form 2')),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _form2Key,
          child: Column(
            children: [
              const Text('Form 2 Entries'),
              Padding(
                padding: const EdgeInsets.symmetric(vertical: 8.0),
                child: TextFormField(
                  controller: _form2Controller1,
                  decoration:
                      const InputDecoration(labelText: 'Form 2 Entry 1'),
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
                  controller: _form2Controller2,
                  decoration:
                      const InputDecoration(labelText: 'Form 2 Entry 2'),
                  validator: (value) {
                    if (value == null || value.isEmpty) {
                      return 'Please enter some text';
                    }
                    return null;
                  },
                ),
              ),
              const SizedBox(height: 20),
            ],
          ),
        ),
      ),
      bottomNavigationBar: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            ElevatedButton(
              onPressed: () => Navigator.pop(context),
              child: const Icon(Icons.arrow_back),
            ),
            ElevatedButton(
              onPressed: () => _submitAllForms(context),
              child: const Text('Finish'),
            ),
          ],
        ),
      ),
    );
  }

  @override
  void dispose() {
    _form2Controller1.dispose();
    _form2Controller2.dispose();
    super.dispose();
  }
}
