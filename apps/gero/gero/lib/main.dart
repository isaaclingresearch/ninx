import 'package:flutter/material.dart';

import 'package:url_launcher/url_launcher.dart';
import 'package:fluttertoast/fluttertoast.dart';

void main() {
  runApp(const Gero());
}
 
class Gero extends StatelessWidget {
  const Gero({super.key});

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Gero',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
        useMaterial3: true,
      ),
      home: const Home(),
    );
  }
}

class UrlButton extends StatelessWidget {
  final String title;
  final String path;

  const UrlButton({super.key, required this.title, required this.path});

  @override
  Widget build(BuildContext context) {
    return TextButton(
      onPressed: () async {
        final uri = Uri.parse(path);
        if (!await launchUrl(uri)) {
          // Show a toast if the URL can't be launched
          Fluttertoast.showToast(
            msg: 'Could not launch $path',
            toastLength: Toast.LENGTH_SHORT,
            gravity: ToastGravity.BOTTOM,
            backgroundColor: Colors.red,
            textColor: Colors.white,
          );
        }
      },
      child: Text(title),
    );
  }
}

class GeroDrawer extends StatelessWidget {
  const GeroDrawer({super.key});

  @override
  Widget build(BuildContext context) {
    return Drawer(
        child: ListView(padding: EdgeInsets.zero, children: [
      const DrawerHeader(
        decoration: BoxDecoration(
          color: Colors.blue,
        ),
        child: Text('Gero Menu'),
      ),
      ListTile(
        title: const Text('Home'),
        onTap: () {
          Navigator.push(
              context, MaterialPageRoute(builder: (context) => const Home()));
        },
      ),
      // ListTile(
      //   title: const Text('About'),
      //   onTap: () {
      //     Navigator.push(
      //         context, MaterialPageRoute(builder: (context) => const About()));
      //   },
      // ),
      // ListTile(
      //   title: const Text('Terms and Privacy'),
      //   onTap: () {
      //     Navigator.push(context,
      //         MaterialPageRoute(builder: (context) => const TermsAndPrivacy()));
      //   },
      // ),
      ListTile(
        title: const Text('Contact Us'),
        onTap: () {
          Navigator.push(context,
              MaterialPageRoute(builder: (context) => const Contacts()));
        },
      )
    ]));
  }
}

class GeroMenu extends StatelessWidget {
  const GeroMenu({super.key});

  @override
  Widget build(BuildContext context) {
    return Builder(
      builder: (context) {
        return IconButton(
          icon: const Icon(Icons.menu),
          onPressed: () {
            Scaffold.of(context).openDrawer();
          },
        );
      },
    );
  }
}

class Home extends StatefulWidget {
  const Home({super.key,});
  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  int _selectedIndex = 0; // to track selected index

  // List of screens for each tab
  static List<Widget> _widgetOptions = <Widget>[
    Text('Home Screen'),
    Text('Search Screen'),
    Text('Profile Screen'),
  ];

  void _onItemTapped(int index) {
    setState(() {
      _selectedIndex = index;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      bottomNavigationBar: BottomNavigationBar(
        items: const <BottomNavigationBarItem>[
          BottomNavigationBarItem(icon: Icon(Icons.home), label: 'Home'),
          BottomNavigationBarItem(
              icon: Icon(Icons.bookmark), label: 'Bookmarks'),
          BottomNavigationBarItem(
              icon: Icon(Icons.menu), label: 'Settings'),
        ],
        currentIndex: _selectedIndex,
        onTap: _onItemTapped,
      ),
      appBar: AppBar(
        title: const Text('Home'),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        leading: const GeroMenu(),
      ),
      drawer: const GeroDrawer(),
      body: Center(
        child:
            _widgetOptions.elementAt(_selectedIndex), // Display selected screen
      ),
    );
  }
}

class Contacts extends StatelessWidget{

  const Contacts({super.key});
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Contact Us'),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        leading: const GeroMenu(),
      ),
      drawer: const GeroDrawer(),
      body: const Padding(
        padding: EdgeInsets.all(5.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text('Email us on:'),
            UrlButton(title: 'info@ninx.xyz', path: 'mailto:info@ninx.xyz'),
          ],
        ),
      ),
    );

  }
}
