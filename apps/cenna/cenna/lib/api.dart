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
    } finally {}
  }

  Future<String> saveAllergy(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-allergy-history'),
        body: {'user-id': userId, 'allergies': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save allergy history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }
  Future<String> saveSocialHistory(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-social-history'),
        body: {'user-id': userId, 'history': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save allergy history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }

  Future<String> saveChronicDisease(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-chronic-disease-history'),
        body: {'user-id': userId, 'chronic-diseases': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save chronic disease history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }

  Future<String> saveMedication(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-medication-history'),
        body: {'user-id': userId, 'medications': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save chronic disease history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }

  Future<String> saveAdmission(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-admission-history'),
        body: {'user-id': userId, 'admissions': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save admission history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }

  Future<String> saveOperation(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-operation-history'),
        body: {'user-id': userId, 'operations': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save admission history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }

  Future<String> saveTransfusion(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-transfusion-history'),
        body: {'user-id': userId, 'transfusions': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save admission history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }

  Future<String> saveTrafficAccident(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-traffic-accident-history'),
        body: {'user-id': userId, 'accidents': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save admission history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }

  Future<String> saveFamilyChronicDisease(String userId, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-family-chronic-disease-history'),
        body: {'user-id': userId, 'diseases': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save admission history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }


  Future<String> saveHistory(String type, String data) async {
    try {
      var response = await client.post(Uri.https('cenna:8443', '/save-history'),
        body: {'type': type, 'data': data});

      if (response.statusCode == 200) {
        return jsonDecode(response.body)['result'];
      } else {
        throw HttpException(
          'Failed to save history: Status ${response.statusCode}, Body: ${response.reasonPhrase}');
      }
    } catch (e) {
      rethrow;
    }
  }
}
