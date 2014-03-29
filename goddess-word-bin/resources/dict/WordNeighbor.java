import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class WordNeighbor {
	private static final int ERROR_NO = -1;

	/***
	 * calculate levenshteinDistance of two string.
	 * 
	 * @param source string1
	 * @param sink string2
	 * @return levenshtein distance
	 */
	public static int levenshteinDistance(String source, String sink)
	{
	    // degenerate cases
	    if (source.equals(sink)) return 0;
	    if (source.length() == 0) return sink.length();
	    if (sink.length() == 0) return source.length();
	 
	    // create two work vectors of integer distances
	    int[] sourceDistances = new int[sink.length() + 1];
	    int[] sinkDistances = new int[sink.length() + 1];
	 
	    // initialize sourceDistances(the previous row of distances)
	    // this row is A[0][i]: edit distance for an empty source
	    // the distance is just the number of characters to delete from sink
	    for (int i = 0; i < sourceDistances.length; i++)
	        sourceDistances[i] = i;
	 
	    for (int i = 0; i < source.length(); i++)
	    {
	        // calculate sinkDistances (current row distances) from the previous row sourceDistances
	 
	        // first element of sinkDistances is A[i+1][0]
	        //   edit distance is delete (i+1) chars from s to match empty t
	        sinkDistances[0] = i + 1;
	 
	        // use formula to fill in the rest of the row
	        for (int j = 0; j < sink.length(); j++)
	        {
	            int cost = (source.charAt(i) == sink.charAt(j)) ? 0 : 1;
	            sinkDistances[j + 1] = minimum(sinkDistances[j] + 1, sourceDistances[j + 1] + 1, sourceDistances[j] + cost);
	        }
	 
	        // copy sinkDistances (current row) to sourceDistances (previous row) for next iteration
	        for (int j = 0; j < sourceDistances.length; j++)
	            sourceDistances[j] = sinkDistances[j];
	    }
	 
	    return sinkDistances[sink.length()];
	}
	
	private static int minimum(int a,int b,int c){
		return Math.min(Math.min(a,b),Math.min(b,c));
	}
	
	private static void assertMsg(boolean expr,String msg){
		if(!expr){
			System.err.println("err:"+msg);
			System.exit(ERROR_NO);
		}
	}
	
	private static void updateWordNeighbors(
			HashMap<String, HashMap<String, Integer>> levDicts, String word1,
			String word2, int distance) {
		HashMap<String,Integer> neighbors=null;
		if(levDicts.containsKey(word1)){
			neighbors= levDicts.get(word1);														
		}else{
			neighbors=new HashMap<String,Integer>();
		}
		assertMsg(neighbors!=null,"neighbors should not be null.");
		neighbors.put(word2, distance);
		levDicts.put(word1, neighbors);		
	}
	
	public static void main(String[] args){  	
		final int NEIGHBORHOOD_DISTANCE=2;
		try {			
			BufferedReader reader=new BufferedReader(new InputStreamReader(Thread.currentThread().getContextClassLoader().getResourceAsStream("en-zh.txt"),Charset.forName("UTF-8")));
			List<String> words = new ArrayList<String>();
			String  line;
			while((line=reader.readLine())!=null){
				if(line.charAt(0)!='#'){// ignore the explaination line.
					String[] wordAndMeaning=line.split("\t");
					assertMsg(wordAndMeaning.length==2,line);
					words.add(wordAndMeaning[0]);
				}else{
					System.err.println("Waring. pass line:"+line);					
				}				
			}
			reader.close();
			
			HashMap<String,HashMap<String,Integer>> levDicts=new HashMap<String,HashMap<String,Integer>>();
			for(int i=0;i<words.size();++i){
				for(int j=(i+1);j<words.size();++j){
					String word1=words.get(i);
					String word2=words.get(j);
					int distance=levenshteinDistance(word1,word2);
					if(distance<=NEIGHBORHOOD_DISTANCE){
						updateWordNeighbors(levDicts,word1,word2,distance);
						updateWordNeighbors(levDicts,word2,word1,distance);
					}
				}
			}
			for(String word:levDicts.keySet()){
				System.out.print(word+":");
				for(String neighbor:levDicts.get(word).keySet()){					
					System.out.print(neighbor+"=>("+levDicts.get(word).get(neighbor)+")");
				}
				System.out.println();
			}
		} catch (Exception e) {
			System.err.println("err:"+e.getMessage());
			System.exit(ERROR_NO);
		}
	}	
}
