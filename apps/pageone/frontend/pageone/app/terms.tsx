import {Text, View, StyleSheet} from 'react-native';

export default About = () => {
  return (
    <View style={styles.container}>
      <Text style={{fontSize: 20}}>
			  These terms are valid after 9th Dec, 2024 and are for the PageOne application (the application) prepared by Ninx Technology Limited (the company). These terms can be accessed online at https://pageone.ninx.xyz/privacy.txt
      </Text>
      <Text style={{fontSize: 20, fontWeight: 'bold',}}>Cost of access:</Text>
      <Text style={{fontSize: 20, marginBottom: 3,}}>The application is free of charge. No amount of money will be charged for access to the product.</Text>
      <Text style={{fontSize: 20, fontWeight: 'bold'}}>Used images:</Text>
      <Text style={{fontSize: 20, marginBottom: 3,}}>The first page images used don't infringe upon the publishers rights, but if you're a publisher and have a complaint about any images used. Get in touch with us at info@ninx.xyz.</Text>
      <Text style={{fontSize: 20, fontWeight: 'bold'}}>Data collecton:</Text>
      <Text style={{fontSize: 20, marginBottom: 3,}}>The application collects a limited amount of data: application crushes and in app speed metrics to improve performance. No personal data is collected. The data collected can not be used to identify the user in any way.</Text>
      <Text style={{fontSize: 20, fontWeight: 'bold'}}>Ads:</Text>
      <Text style={{fontSize: 20, marginBottom: 3,}}>The company funds the service through showing ads in between the pages. We try to use ads that don't interfere with in-app activity.</Text>
      
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
  }});
