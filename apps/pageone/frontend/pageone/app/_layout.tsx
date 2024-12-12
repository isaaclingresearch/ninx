import { GestureHandlerRootView } from 'react-native-gesture-handler';
import { Drawer } from 'expo-router/drawer';

export default function Layout() {
	return (
	  <GestureHandlerRootView style={{ flex: 1,}}>
			<Drawer>
				<Drawer.Screen
					name="index"
					options={{
						drawerLabel: 'Home',
						title: 'PageOne',
					}}
				/>
				<Drawer.Screen
					name="about"
					options={{
						drawerLabel: 'About',
						title: 'About',
					}}
				/>
				<Drawer.Screen
					name="terms"
					options={{
						drawerLabel: 'Terms and Privacy',
						title: 'Terms and Privacy',
					}}
				/>
				<Drawer.Screen
					name="contact"
					options={{
						drawerLabel: 'Contact Us',
						title: 'Our Contacts',
					}}
				/>

			</Drawer>
    </GestureHandlerRootView>
  );
}
