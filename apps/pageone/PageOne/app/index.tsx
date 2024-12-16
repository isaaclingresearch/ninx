import { useState, useEffect } from 'react';
import {Text, View, StyleSheet, Image} from 'react-native';
import { FlatList } from "react-native-bidirectional-infinite-scroll";

const Img = ({ image }) => {
  return (
    <Image source={{uri: `https://10.0.2.2:8433/get-image?id=${image.id}`}} />
  );
}
export default function Index() {
  
  const [page, setPage] = useState(1);
  const [listData, setListData] = useState([]);

  // useEffect(() => {
  //   try {
  //   const getInitialImages = async () => {
  //     const response = await fetch('https://pageone.ninx:8433/get-images?page=1');
  //     const initialImages = await response.json();
  //     if (initialImages == []) return;
  //     setListData(initialImages.data || []);
  //   };
  //   } catch (e){
  //     console.log(e)
  //   }

  //   getInitialImages();
  // }, []);

  useEffect(() => {
    const getInitialImages = async () => {
      try {
	const response = await fetch('https://10.0.2.2:8433/get-images?page=1');

	if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
	}

	const initialImages = await response.json();

	// Check if `initialImages.data` is empty
	if (!initialImages.data || initialImages.data.length === 0) {
          console.log("No images found");
          return;
	}

	setListData(initialImages.data);
      } catch (error) {
	console.error("Error fetching initial images:", error);
      }
    };

    getInitialImages();
  }, []);


  const onStartReached = async () => {
    const response = await fetch('https://10.0.2.2:8433/get-images?page=1');
    const data = await response.json();
    setListData((d) => {
      return [...new Set(data.data.concat(d))];
    });
  }

  const onEndReached = async () => {
    setPage((prevPage) => prevPage + 1);
    const response = await fetch(`https://10.0.2.2:8433/get-images?page=${page + 1}`);
    const data = await response.json();
    setListData((d) => d.concat(data.data));
  };

  return (
    <FlatList
      data={listData}
      renderItem={({ item }) => <Img image={item} />}
      keyExtractor={(item) => item.id}
      onStartReached={onStartReached} // required, should return a promise
      onEndReached={onEndReached} // required, should return a promise
      showDefaultLoadingIndicators={true} // optional
      onStartReachedThreshold={10} // optional
      onEndReachedThreshold={10} // optional
      activityIndicatorColor={'black'} // optional
      HeaderLoadingIndicator={() => { /** Your loading indicator */ }} // optional
      FooterLoadingIndicator={() => { /** Your loading indicator */ }} // optional
      enableAutoscrollToTop={false} // optional | default - false
      // You can use any other prop on react-native's FlatList
    />
  );
}


const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#1e90ff',
    alignItems: 'center',
    justifyContent: 'center',
  },
  text: {
    color: 'black',
  },
});
