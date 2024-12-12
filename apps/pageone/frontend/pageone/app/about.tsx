import {Text, View, StyleSheet} from 'react-native';

export default About = () => {
  return (
    <View style={styles.container}>
      <Text style={{fontSize: 20, fontWeight: 'bold', marginBottom: 5}}>PageOne</Text>
      <Text style={{fontSize: 20, marginBottom: 5}}>PageOne is a mobile application that brings you the first page of news papers in Uganda.</Text>
      <Text style={{fontSize: 20, marginBottom: 5,}}>The app is built, maintained and marketed by <Text style={{fontWeight: 'bold'}}>Ninx Technology Limited.</Text></Text>
      <Text style={{fontSize: 20, marginBottom: 5,}}>Email us on <Text style={{fontWeight: 'bold'}}>info@ninx.xyz</Text></Text>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: 'ffffff',
    padding: '2%',
  },
  text: {
    color: '#fff',
    fontSize: 100,
  }});
