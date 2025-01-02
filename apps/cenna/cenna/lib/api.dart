part of 'main.dart';

class NextOfKinData {
  final String name;
  final String email;
  final String relationship;
  final String telephoneNumber;

  NextOfKinData({
    required this.name,
    required this.email,
    required this.relationship,
    required this.telephoneNumber,
  });

  Map<String, dynamic> toMap() {
    return {
      'next-of-kin-name': name,
      'next-of-kin-email': email,
      'next-of-kin-relationship': relationship,
      'next-of-kin-telephone-number': telephoneNumber,
    };
  }

  // Create a NextOfKinData object from a map
  factory NextOfKinData.fromMap(Map<String, dynamic> map) {
    return NextOfKinData(
      name: map['next-of-kin-name'],
      email: map['next-of-kin-email'],
      relationship: map['next-of-kin-relationship'],
      telephoneNumber: map['next-of-kin-telephone-number'],
    );
  }

  void saveToDb(userId) {
    DbHandle db = DbHandle();
    var data = toMap();
    for (var key in toMap().keys) {
      db.setDemographic(userId, key, data[key]);
    }
    db.close();
  }
}

class DemographicsData {
  final String email;
  final String sexAtBirth;
  final String race;
  final String maritalStatus;
  final String occupation;
  final String fullName;
  final String telephoneNumber;
  final DateTime dateOfBirth;
  final String gender;
  final String levelOfEducation;
  final String countryOfOrigin;
  final String cityOfResidence;
  final String countryOfResidence;
  final NextOfKinData nextOfKin;

  DemographicsData({
    required this.email,
    required this.sexAtBirth,
    required this.race,
    required this.maritalStatus,
    required this.occupation,
    required this.fullName,
    required this.telephoneNumber,
    required this.dateOfBirth,
    required this.gender,
    required this.levelOfEducation,
    required this.countryOfOrigin,
    required this.nextOfKin,
    required this.countryOfResidence,
    required this.cityOfResidence,
  });

  Map<String, dynamic> toMap() {
    return {
      'email': email,
      'sex-at-birth': sexAtBirth,
      'race': race,
      'marital-status': maritalStatus,
      'occupation': occupation,
      'full-name': fullName, // Corrected key name to match fromMap
      'telephone-number': telephoneNumber,
      'date-of-birth': DateFormat('yyyy-mm-dd').format(dateOfBirth),
      'gender': gender,
      'level-of-education': levelOfEducation,
      'country-of-origin': countryOfOrigin, // Corrected key name
      'next-of-kin': nextOfKin.toMap(),
    };
  }

  String toJson() {
    return jsonEncode(toMap());
  }

  // Create a DemographicsData object from a map
  factory DemographicsData.fromMap(Map<String, dynamic> map) {
    return DemographicsData(
      countryOfResidence: map['country-of-residence'],
      cityOfResidence: map['city-of-residence'],
      email: map['email'],
      sexAtBirth: map['sex-at-birth'],
      race: map['race'],
      maritalStatus: map['marital-status'],
      occupation: map['occupation'],
      fullName: map['full-name'], // Corrected key name
      telephoneNumber: map['telephone-number'],
      dateOfBirth: DateFormat('yyyy-mm-dd').parse(map['date-of-birth']),
      gender: map['gender'],
      levelOfEducation: map['level-of-education'],
      countryOfOrigin: map['country-of-origin'], // Corrected key name
      nextOfKin: NextOfKinData.fromMap(map['next-of-kin']),
    );
  }

  void saveToDb(userId) {
    DbHandle db = DbHandle();
    var data = toMap();
    for (var key in data.keys) {
      if (key == 'next-of-kin') {
        // Save the NextOfKinData data separately
        nextOfKin.saveToDb(userId);
      } else {
        db.setDemographic(userId, key, data[key]);
      }
    }
    db.setCurrentUserId(userId);
    db.setRegisteredUserId(userId);
    db.close();
  }
}

class Api {
  Api();

  void saveDemographics(DemographicsData data) async {
    DbHandle db = DbHandle();
    var client = http.Client();
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
      client.close();
    }
  }
}
