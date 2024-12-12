import 'package:flutter/material.dart';
import 'package:url_launcher/url_launcher.dart';
import 'package:fluttertoast/fluttertoast.dart';
import 'package:infinite_scroll_pagination/infinite_scroll_pagination.dart';
import 'package:cached_network_image/cached_network_image.dart';
import 'package:dio/dio.dart';

final dio = Dio();
void main() {
  runApp(const PageOne());
}

class PageOne extends StatelessWidget {
  const PageOne({super.key});

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'PageOne',
      initialRoute: '/',
      routes: {
        '/': (context) => const Home(),
        '/about': (context) => const About(),
        '/terms-and-privacy': (context) => const TermsAndPrivacy(),
        '/contacts': (context) => const Contacts()
      },
      theme: ThemeData(
        // This is the theme of your application.
        //
        // TRY THIS: Try running your application with "flutter run". You'll see
        // the application has a purple toolbar. Then, without quitting the app,
        // try changing the seedColor in the colorScheme below to Colors.green
        // and then invoke "hot reload" (save your changes or press the "hot
        // reload" button in a Flutter-supported IDE, or press "r" if you used
        // the command line to start the app).
        //
        // Notice that the counter didn't reset back to zero; the application
        // state is not lost during the reload. To reset the state, use hot
        // restart instead.
        //
        // This works for code too, not just values: Most code changes can be
        // tested with just a hot reload.
        colorScheme: ColorScheme.fromSeed(seedColor: const Color(0xFF1E90FF)),
        useMaterial3: true,
      ),
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

class About extends StatelessWidget {
  const About({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('About'),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        leading: const PageOneMenu(),
      ),
      drawer: const PageOneDrawer(),
      body: const Padding(
        padding: EdgeInsets.all(5.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'PageOne',
              style: TextStyle(fontWeight: FontWeight.bold),
            ),
            SizedBox(height: 10),
            Text(
              'PageOne is a mobile application that shows you what is in the newspapers of Uganda everyday.\n\n'
              'The app is built, maintained, and marketed by Ninx Technology Limited.\n\n',
            ),
            Text(
              'Email us on:',
            ),
            UrlButton(
              title: 'info@ninx.xyz',
              path: 'mailto:info@ninx.xyz',
            ),
          ],
        ),
      ),
    );
  }
}

class TermsAndPrivacy extends StatelessWidget {
  const TermsAndPrivacy({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Terms and Privacy'),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        leading: const PageOneMenu(),
      ),
      drawer: const PageOneDrawer(),
      body: const SingleChildScrollView(
        child: Padding(
          padding: EdgeInsets.all(5.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                  'These terms are valid after 9th Dec, 2024 and are for the PageOne application (the application) prepared by Ninx Technology Limited (the company). These terms can be accessed online at:'),
              UrlButton(
                  title: 'https://pageone.ninx.xyz/privacy.txt',
                  path: 'https://pageone.ninx.xyz/privacy.txt'),
              SizedBox(height: 10),
              Text('Cost of access:',
                  style: TextStyle(fontWeight: FontWeight.bold)),
              Text(
                  'The application is free of charge. No amount of money will be charged for access to the product.\n\n'),
              Text('Developers:',
                  style: TextStyle(fontWeight: FontWeight.bold)),
              Text(
                  "The application is developed, maintained, and marketed by the company's mobile applications division. Any official communication to the company or the division can be done via:"),
              UrlButton(title: 'info@ninx.xyz', path: 'mailto:info@ninx.xyz'),
              SizedBox(height: 10),
              Text('Used images:',
                  style: TextStyle(fontWeight: FontWeight.bold)),
              Text(
                  "The first page images used don't infringe upon the publisher's rights, but if you're a publisher and have a complaint about any images used, get in touch with us at:"),
              UrlButton(title: 'info@ninx.xyz', path: 'mailto:info@ninx.xyz'),
              SizedBox(height: 10),
              Text('Data collection:',
                  style: TextStyle(fontWeight: FontWeight.bold)),
              Text(
                  "The application collects a limited amount of data: application crashes and in-app speed metrics to improve performance. No personal data is collected. The data collected cannot be used to identify the user in any way.\n\n"),
              Text('Ads:', style: TextStyle(fontWeight: FontWeight.bold)),
              Text(
                  "The company funds the service through showing ads in between the pages. We try to use ads that don't interfere with in-app activity.\n\n"),
            ],
          ),
        ),
      ),
    );
  }
}

class Contacts extends StatelessWidget {
  const Contacts({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Contact Us'),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        leading: const PageOneMenu(),
      ),
      drawer: const PageOneDrawer(),
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

class PageOneDrawer extends StatelessWidget {
  const PageOneDrawer({super.key});

  @override
  Widget build(BuildContext context) {
    return Drawer(
        child: ListView(padding: EdgeInsets.zero, children: [
      const DrawerHeader(
        decoration: BoxDecoration(
          color: Colors.blue,
        ),
        child: Text('PageOne Menu'),
      ),
      ListTile(
        title: const Text('Home'),
        onTap: () {
          Navigator.push(
              context, MaterialPageRoute(builder: (context) => const Home()));
        },
      ),
      ListTile(
        title: const Text('About'),
        onTap: () {
          Navigator.push(
              context, MaterialPageRoute(builder: (context) => const About()));
        },
      ),
      ListTile(
        title: const Text('Terms and Privacy'),
        onTap: () {
          Navigator.push(context,
              MaterialPageRoute(builder: (context) => const TermsAndPrivacy()));
        },
      ),
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

class PageOneMenu extends StatelessWidget {
  const PageOneMenu({super.key});

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
  const Home({super.key});

  // This widget is the home page of your application. It is stateful, meaning
  // that it has a State object (defined below) that contains fields that affect
  // how it looks.

  // This class is the configuration for the state. It holds the values (in this
  // case the title) provided by the parent (in this case the App widget) and
  // used by the build method of the State. Fields in a Widget subclass are
  // always marked "final".

  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {

  void _incrementCounter() {
    setState(() {
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        title: const Text('PageOne'),
        leading: const PageOneMenu(),
      ),
      drawer: const PageOneDrawer(),
      body: const PaperListView(),
      floatingActionButton: FloatingActionButton(
        onPressed: _incrementCounter,
        tooltip: 'Increment',
        child: const Icon(Icons.add),
    ),);
  }
}

class PaperListView extends StatefulWidget {
  const PaperListView({super.key});

  @override
  State<StatefulWidget> createState() => _PaperListViewState();
}

class _PaperListViewState extends State<PaperListView> {
  static const _pageSize = 10;

  final PagingController<int, Paper> _pagingController = PagingController(firstPageKey: 1);

  @override
  void initState() {
    super.initState();
    _pagingController.addPageRequestListener((pageKey) => _fetchPaper(pageKey));
  }

  Future<void> _fetchPaper(pageKey) async {
    try {
      final response = await dio.get('https://pageone.ninx:8443/get-images', queryParameters: {'page': pageKey});
      if (response.statusCode == 200){
        final data = response.data;
        if (data == false){
          _pagingController.appendPage([], 1);
        }else if (data.length < _pageSize){
          _pagingController.appendLastPage(data.map<Paper>((datum) => Paper(data: datum)).toList());
        }else{
          _pagingController.appendPage(data.map<Paper>((datum) => Paper(data: datum,)).toList(), pageKey+1);
        }
      } else {
        throw Exception('Failed to load papers.');
      }
    } catch (error) {
      _pagingController.error = error;
    }
  }

  @override
  Widget build(BuildContext context) =>
  PagedListView<int, Paper>(
    pagingController: _pagingController,
    builderDelegate: PagedChildBuilderDelegate<Paper>(
      itemBuilder: (context, paper, index) => PaperListItem(paper: paper)
    ),
  );

  @override
  void dispose(){
    _pagingController.dispose();
    super.dispose();
  }
}

class Paper extends StatelessWidget {
  final Map<String, dynamic> data;

  const Paper({super.key, required this.data});

  @override
  Widget build(BuildContext context) {
    return CachedNetworkImage(
       imageUrl: data['url'],
       progressIndicatorBuilder: (context, url, downloadProgress) =>
               Center(child: SizedBox(width: 50.0, height: 50.0, child: CircularProgressIndicator(value: downloadProgress.progress, ))),
       errorWidget: (context, url, error) => const Icon(Icons.error),
    );
  }
}

class PaperListItem extends StatelessWidget {
  final Paper paper;
  const PaperListItem({super.key, required this.paper});

  @override
  Widget build(BuildContext context) {
    return paper;
  }
}
