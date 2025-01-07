import 'dart:convert';
import 'dart:ffi';
import 'dart:io';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';

import 'package:intl/intl.dart'; // For date formatting
import 'package:country_picker/country_picker.dart';
import 'package:intl_phone_field/intl_phone_field.dart';
import 'package:intl_phone_field/phone_number.dart';
import 'package:path/path.dart';
import 'package:sqlite3/open.dart';
import 'package:sqlite3/sqlite3.dart' hide Row;
import 'package:path_provider/path_provider.dart';
import 'package:uuid/uuid.dart';
import 'package:http/http.dart' as http;
import 'package:multi_select_flutter/multi_select_flutter.dart';
import 'package:flutter_chat_types/flutter_chat_types.dart' as types;
import 'package:flutter_chat_ui/flutter_chat_ui.dart';

part 'api.dart';
part 'db.dart';
part 'demographics.dart';

// history parts
part 'history/traffic_accidents.dart';
part 'history/transfusion.dart';
part 'history/operation.dart';
part 'history/admission.dart';
part 'history/chronic_disease.dart';
part 'history/allergy.dart';
part 'history/medication.dart';
part 'history/family_chronic_disease.dart';

var uuid = Uuid();
var dev = false;

var dbName = dev ? '/cenna${uuid.v4()}.db' : '/cenna.db';
String dbPath = '';

DynamicLibrary _openOnLinux() {
  final scriptDir = File(Platform.script.toFilePath()).parent;
  final libraryNextToScript = File(join(scriptDir.path, 'libsqlite3.so.0'));
  return DynamicLibrary.open(libraryNextToScript.path);
}

void main() async {
  open.overrideFor(OperatingSystem.linux, _openOnLinux);
  final directory = Platform.isIOS
      ? await getLibraryDirectory()
      : await getApplicationDocumentsDirectory();
  dbPath = '${directory.path}$dbName';
  runApp(Cenna());
}

class Cenna extends StatefulWidget {
  const Cenna({super.key});

  @override
  State<Cenna> createState() => _Cenna();
}

class _Cenna extends State<Cenna> {
  late DbHandle db;

  @override
  initState() {
    super.initState();
    db = DbHandle();
    db.createTables();
  }

  @override
  void dispose() {
    db.close();
    super.dispose();
  }

  // check for active user_id, if its in the registered users table, it means it has been saved go to
  // Home, otherwise, for now go to the demographics with the incomplete user_id
  String _checkActiveUserType() {
    String? activeId = db.getCurrentUserId();
    String finalResult = '';
    if (activeId == null) {
      finalResult = 'start-registration';
    } else if (db.isUserIdRegistered(activeId)) {
      finalResult = 'go-home';
    } else {
      finalResult = 'continue-registration';
    }
    return finalResult;
  }

  Widget? home;
  @override
  Widget build(BuildContext context) {
    var action = _checkActiveUserType();
    switch (action) {
      case 'start-registration':
        home = DemographicsForm();
        break;
      case 'continue-registration':
        home = DemographicsForm();
        break;
      case 'go-home':
        home = Home();
    }

    return MaterialApp(
      title: 'Cenna',
      home: AllergiesForm(),
    );
  }
}

String? validateEmail(String? value) {
  if (value == null || value.isEmpty) {
    return 'Please enter an email address.';
  }

  // Regular expression for email validation
  final emailRegex = RegExp(
      r"^[a-zA-Z0-9.a-zA-Z0-9.!#$%&'*+-/=?^_`{|}~]+@[a-zA-Z0-9]+\.[a-zA-Z]+");

  if (!emailRegex.hasMatch(value)) {
    return 'Please enter a valid email address.';
  }

  return null; // Return null if the email is valid
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

class Home extends StatelessWidget {
  const Home({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Form 2')),
      body: const Text('hello'),
    );
  }
}
