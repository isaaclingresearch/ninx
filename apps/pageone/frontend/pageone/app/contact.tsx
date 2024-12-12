import {Text, View, StyleSheet} from 'react-native';

export default Contact = () => {
  return (
    <View style={styles.container}>
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
