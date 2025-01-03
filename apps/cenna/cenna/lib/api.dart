part of 'main.dart';

class Api {
  late http.Client client;

  Api() {
    client = http.Client();
  }

  void close() {
    client.close();
  }

  void saveDemographics(DemographicsData data) async {
    DbHandle db = DbHandle();
    try {
      var response = await client.post(
          Uri.https('cenna:8443', '/save-user-profile'),
          body: {'profile-json': data.toJson()});
      var id = jsonDecode(response.body)['user-id'];
      // when the id is returned, save it and the name to registered-users table.
      // then save the data to the registered-users table and make it active
      // then redirect to home.
      db.setRegisteredUserId(id);
      db.setCurrentUserId(id);
      db.close();
    } finally {
    }
  }
}
