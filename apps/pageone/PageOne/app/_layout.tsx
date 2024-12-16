import { GestureHandlerRootView } from 'react-native-gesture-handler';
import { Drawer } from 'expo-router/drawer';

export default function RootLayout() {
  return (
    <GestureHandlerRootView style={{ flex: 1 }}>
      <Drawer>
	<Drawer.Screen name="index" options={{drawerLabel: "PageOne", title: "PageOne"}} />
	<Drawer.Screen name="about" options={{drawerLabel: "About", title: "About"}} />
	<Drawer.Screen name="contacts" options={{drawerLabel: "Contacts", title: "Contacts"}} />
	<Drawer.Screen name="terms" options={{drawerLabel: "Terms and Privacy", title: "Terms and Privacy"}} />
      </Drawer>
    </GestureHandlerRootView>

  );
}
