part of 'main.dart';

DynamicLibrary _openOnLinux() {
  final scriptDir = File(Platform.script.toFilePath()).parent;
  final libraryNextToScript = File(join(scriptDir.path, 'libsqlite3.so.0'));
  return DynamicLibrary.open(libraryNextToScript.path);
}

class DbHandle {
  late Database db;
  _initialiseDb() async {
    final directory = Platform.isIOS
        ? await getLibraryDirectory()
        : await getApplicationDocumentsDirectory();
    String path = '${directory.path}$dbName';
    db = sqlite3.open(path);
  }

  DbHandle() {
    _initialiseDb();
  }

  void close() {
    db.dispose();
  }

  void createTables() {
    db.execute('''
    CREATE TABLE IF NOT EXISTS system_variables (
      variable text primary key,
      value text,
      set_date text default CURRENT_TIMESTAMP);

    CREATE TABLE IF NOT EXISTS user_ids (
      user_id text primary key,
      created_at default CURRENT_TIMESTAMP);

    CREATE TABLE IF NOT EXISTS registered_users (
      user_id text primary key,
      created_at default CURRENT_TIMESTAMP);

    CREATE TABLE IF NOT EXISTS registered_demographics (
      demographic TEXT,
      value TEXT,
      user_id TEXT,
      set_date DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (demographic, user_id)
      FOREIGN KEY (user_id) REFERENCES registered_users(user_id));

      CREATE TABLE IF NOT EXISTS history (
       user_id TEXT,
       type TEXT,
       data TEXT,
       PRIMARY KEY (user_id, type)
       FOREIGN KEY (user_id) REFERENCES registered_users(user_id));
       
    CREATE TABLE IF NOT EXISTS demographics (
      demographic TEXT,
      value TEXT,
      user_id TEXT,
      set_date DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (demographic, user_id)
      FOREIGN KEY (user_id) REFERENCES user_ids(user_id));

      CREATE TABLE IF NOT EXISTS next_of_kin (
      id primary key autoincrement,
      name text,
      email text,
      telephone_number text,
      user_id text,
      FOREIGN KEY (user_id) REFERENCES user_ids(user_id));
    ''');
  }

  // system variables hold global information such as the current user.
  String? getSystemVariable(variable) {
    final query =
        db.prepare('''select value from system_variables where variable=?''');
    final ResultSet entry = query.select([variable]);
    query.dispose();
    return entry.isEmpty ? null : entry[0]['value'];
  }

  void setSystemVariable(variable, value) {
    final query = db.prepare(
        '''insert or replace into system_variables (variable, value) values (?, ?)''');
    query.execute([variable, value]);
    query.dispose();
  }

  // holds demographic data before it's sent to the server.
  void setDemographic(userId, demographic, value) {
    final query = db.prepare(
        '''insert or replace into demographics (demographic, value, user_id) values (?, ?, ?) ''');
    query.execute([userId, demographic, value]);
    query.dispose();
  }

  void setDemographics(userId, Map data) {
    final query = db.prepare(
        '''insert or replace into demographics (demographic, value, user_id) values (?, ?, ?) ''');

    for (String key in data.keys) {
      query.execute([userId, key, data[key]]);
    }
    query.dispose();
  }

  String? getDemographic(userId, demographic) {
    final query = db.prepare(
        '''select value from demographics where demographic=? and user_id=?''');
    final ResultSet result = query.select([demographic, userId]);
    query.dispose();
    return result.isEmpty ? null : result[0]['value'];
  }

  DemographicsData getInitialDemographics(userId) {
    var nextOfKin = NextOfKinData(
      email: getDemographic(userId, 'next-of-kin-email') ?? '',
      name: getDemographic(userId, 'next-of-kin-name') ?? '',
      telephoneNumber:
          getDemographic(userId, 'next-of-kin-telephone-number') ?? '',
      relationship: getDemographic(userId, 'next-of-kin-relationship') ?? '',
    );
    return DemographicsData(
        nextOfKin: nextOfKin,
        gender: getDemographic(userId, 'gender') ?? '',
        countryOfOrigin: getDemographic(userId, 'country-of-origin') ?? '',
        fullName: getDemographic(userId, 'full-name') ?? '',
        sexAtBirth: getDemographic(userId, 'sex-at-birth') ?? '',
        race: getDemographic(userId, 'race') ?? '',
        levelOfEducation: getDemographic(userId, 'level-of-education') ?? '',
        countryOfResidence:
            getDemographic(userId, 'country-of-residence') ?? '',
        cityOfResidence: getDemographic(userId, 'city-of-residence') ?? '',
        occupation: getDemographic(userId, 'occupation') ?? '',
        telephoneNumber: getDemographic(userId, 'telephone-number') ?? '',
        email: getDemographic(userId, 'email') ?? '',
        maritalStatus: getDemographic(userId, 'marital-status') ?? '',
        dateOfBirth: DateFormat('yyyy-mm-dd')
            .parse(getDemographic(userId, 'date-of-birth') ?? '1800-01-01'));
  }

  // user ids are generated locally to track the user through creation of demographics.
  void setUserId(userId) {
    final query =
        db.prepare('''insert or replace into user_ids (user_id) values (?)''');
    query.execute([userId]);
    query.dispose();
  }

  List getUserIds() {
    final query = db.prepare('''select user_id from user_ids''');
    final ResultSet data = query.select();
    query.dispose();
    return data.map((value) => value['user_id']).toList();
  }

  // registered users are ids returned from sending demographic data to the server.
  void setRegisteredUserId(userId) {
    final query = db.prepare(
        '''insert or replace into registered_users (user_id) values (?)''');
    query.execute([userId]);
    query.dispose();
  }

  List getRegisteredUserIds() {
    final query = db.prepare('''select user_id from registered_users''');
    final ResultSet data = query.select();
    query.dispose();
    return data.map((value) => value['user_id']).toList();
  }

  // check whether a given user id is a registered user. this can be used in navigation to determine
  // appropriate course of action.
  bool isUserIdRegistered(userId) {
    final query =
        db.prepare('''select * from registered_users where user_id=?''');
    final ResultSet data = query.select([userId]);
    query.dispose();
    return data.isNotEmpty;
  }

  String? getCurrentUserId() {
    return getSystemVariable('current_user_id');
  }

  void setCurrentUserId(userId) {
    setSystemVariable('current_user_id', userId);
  }

  // history is saved in bits and pieces, so forexample; to collect the whole, collect the pieces
  // medical = medication, allergy, admission, chronic_illness
  // surgical = surgeries, fractures, blood transfusions.
  // social = religion, drugs, smoking, alcohol, hobbies.
  // family = family illness,

  void setHistory(userId, type, history) {
    final query = db.prepare(
        '''insert or replace into history (user_id, history, data) values (?, ?, ?)''');
    query.execute([userId, type, history]);
    query.dispose();
  }

  String? getHistory(userId, type) {
    final query =
        db.prepare('''select data from history where user_id=? and type=?''');
    final ResultSet results = query.select([userId, type]);
    query.dispose();
    return results.isEmpty ? null : results[0]['data'];
  }

  void setAllergyHistory(userId, data) {
    setHistory(userId, 'allergy', data);
  }

  String? getAllergyHistory(userId) {
    return getHistory(userId, 'allergy');
  }

  void setAdmissionHistory(userId, data) {
    setHistory(userId, 'admission', data);
  }

  String? getAdmissionHistory(userId) {
    return getHistory(userId, 'admission');
  }

  void setMedicationHistory(userId, data) {
    setHistory(userId, 'medication', data);
  }

  String? getMedicationHistory(userId) {
    return getHistory(userId, 'medication');
  }

  void setChronicDiseaseHistory(userId, data) {
    setHistory(userId, 'chronic disease', data);
  }

  String? getChronicDiseaseHistory(userId) {
    return getHistory(userId, 'chronic disease');
  }

  void setSurgicalOperationHistory(userId, data) {
    setHistory(userId, 'surgical operation', data);
  }

  String? getSurgicalOperationHistory(userId) {
    return getHistory(userId, 'surgical operation');
  }

  void setTransfusionHistory(userId, data) {
    setHistory(userId, 'transfusion', data);
  }

  String? getTransfusionHistory(userId) {
    return getHistory(userId, 'transfusion');
  }

  void setFractureHistory(userId, data) {
    setHistory(userId, 'fracture', data);
  }

  String? getFractureHistory(userId) {
    return getHistory(userId, 'fracture');
  }

  void setFamilyIllnessHistory(userId, data) {
    setHistory(userId, 'family illness', data);
  }

  String? getFamilyIllnessHistory(userId) {
    return getHistory(userId, 'family illness');
  }

  void setReligionHistory(userId, data) {
    setHistory(userId, 'religion', data);
  }

  String? getReligionHistory(userId) {
    return getHistory(userId, 'religion');
  }

  void setDrugsHistory(userId, data) {
    setHistory(userId, 'drugs', data);
  }

  String? getDrugsHistory(userId) {
    return getHistory(userId, 'drugs');
  }

  void setHobbiesHistory(userId, data) {
    setHistory(userId, 'hobbies', data);
  }

  String? getHobbiesHistory(userId) {
    return getHistory(userId, 'hobbies');
  }
}
