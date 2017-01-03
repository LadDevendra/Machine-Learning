import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

class Node {	  
	   public String[] attributes = new String[20];
	   public ArrayList<String[]> data = new ArrayList<String[]>();
	   public int number;
	   public Node leftChild;   
	   public Node rightChild;
	   public boolean isLeaf;
	   public String classLabel;
	   public int splitAttribute;
	   public int splittedOn_attribute;
	   public int splittedOn_value;
	   public int depth;
	   
	   public Node()
	   {
		   isLeaf = false;
		   depth = 0;
	   }
	   
	   public Node(Node node)
	   {
		   this.attributes = node.attributes;
		   this.data = node.data;
		   this.number = node.number;
		   this.leftChild = node.leftChild;
		   this.rightChild = node.rightChild;
		   this.isLeaf = node.isLeaf;
		   this.classLabel = node.classLabel;
		   this.splitAttribute = node.splitAttribute;
		   this.splittedOn_attribute = node.splittedOn_attribute;
		   this.splittedOn_value = node.splittedOn_value;
	   }
}

class labelCount {
	
	public labelCount()
	{
		count_Negative = 0;
		count_Positive = 0;
		count_Total = 0;
	}
	public double count_Positive;
	public double count_Negative;
	public double count_Total;
}

class Tree {

	public Node[] parentArray = new Node[100];
	public int[] childArray = new int[100];
	public int index;
	public String[] allAttributes = new String[20];
	int sumLeaf;
	
	public int count;
	
	public Tree()
	{
		count = 0;
		index = 0;
		sumLeaf = 0;
	}
	
	public void id3(Node s, Tree tree)
	{
        int bestAttribIndex = bestAttribute(s);     				//Get best attribute from available choices
        splitNode(s, bestAttribIndex, tree);						//Split the node on that attribute
      
        if(!isPure(s.leftChild))   //node is impure
        {
        	if(isAttributeAvailable(s.leftChild))
        	{
        		id3(s.leftChild, tree);								//Left recursion
        	}
        	else
        	{
        		s.leftChild.isLeaf = true;
        		s.leftChild.classLabel = getClassLabel_leaf(s.leftChild);
        		tree.parentArray[tree.index] = s; 					//saving parent node reference
        		tree.childArray[tree.index] = s.leftChild.number; 	//Saving child node number
        		tree.index += 1;
        		tree.sumLeaf += s.leftChild.depth;
        	}
        }
        else
        {
        	s.leftChild.isLeaf = true;
        	s.leftChild.classLabel = getClassLabel_leaf(s.leftChild);
        	tree.parentArray[tree.index] = s; 						//saving parent node reference
    		tree.childArray[tree.index] = s.leftChild.number; 		//Saving child node number
    		tree.index += 1;
    		tree.sumLeaf += s.leftChild.depth;
        }
       
        if(!isPure(s.rightChild))
        {
        
        	if(isAttributeAvailable(s.rightChild))
        	{
        		id3(s.rightChild, tree);							//Right recursion
        		
        	}
        	else
        	{
        		s.rightChild.isLeaf = true;
        		s.rightChild.classLabel = getClassLabel_leaf(s.rightChild);
        		tree.parentArray[tree.index] = s; 					//saving parent node reference
        		tree.childArray[tree.index] = s.rightChild.number; 	//Saving child node number
        		tree.index += 1;
        		tree.sumLeaf += s.rightChild.depth;
        	}
        }
        else{
        	s.rightChild.isLeaf = true;
        	s.rightChild.classLabel = getClassLabel_leaf(s.rightChild);
        	tree.parentArray[tree.index] = s; 						//saving parent node reference
    		tree.childArray[tree.index] = s.rightChild.number; 		//Saving child node number
    		tree.index += 1;
    		tree.sumLeaf += s.rightChild.depth;
        }
       
	}
	
	public void randomSelection(Node s, Tree tree)
	{
		
        int randomAttribIndex;
        randomAttribIndex = randomAttribute(s);
        
        while(s.attributes[randomAttribIndex] == "0")
        {
        	randomAttribIndex = randomAttribute(s);     				//Get Random attribute from available choices
        }
        	
        splitNode(s, randomAttribIndex, tree);						//Split the node on that attribute
      
        if(!isPure(s.leftChild))   //node is impure
        {
        	if(isAttributeAvailable(s.leftChild))
        	{
        		randomSelection(s.leftChild, tree);								//Left recursion
        	}
        	else
        	{
        		s.leftChild.isLeaf = true;
        		s.leftChild.classLabel = getClassLabel_leaf(s.leftChild);
        		tree.parentArray[tree.index] = s; 					//saving parent node reference
        		tree.childArray[tree.index] = s.leftChild.number; 	//Saving child node number
        		tree.index += 1;
        		tree.sumLeaf += s.leftChild.depth;
        	}
        }
        else
        {
        	s.leftChild.isLeaf = true;
        	s.leftChild.classLabel = getClassLabel_leaf(s.leftChild);
        	tree.parentArray[tree.index] = s; 						//saving parent node reference
    		tree.childArray[tree.index] = s.leftChild.number; 		//Saving child node number
    		tree.index += 1;
    		tree.sumLeaf += s.leftChild.depth;
        }
       
        if(!isPure(s.rightChild))
        {
        
        	if(isAttributeAvailable(s.rightChild))
        	{
        		randomSelection(s.rightChild, tree);							//Right recursion
        		
        	}
        	else
        	{
        		s.rightChild.isLeaf = true;
        		s.rightChild.classLabel = getClassLabel_leaf(s.rightChild);
        		tree.parentArray[tree.index] = s; 					//saving parent node reference
        		tree.childArray[tree.index] = s.rightChild.number; 	//Saving child node number
        		tree.index += 1;
        		tree.sumLeaf += s.rightChild.depth;
        	}
        }
        else{
        	s.rightChild.isLeaf = true;
        	s.rightChild.classLabel = getClassLabel_leaf(s.rightChild);
        	tree.parentArray[tree.index] = s; 						//saving parent node reference
    		tree.childArray[tree.index] = s.rightChild.number; 		//Saving child node number
    		tree.index += 1;
    		tree.sumLeaf += s.rightChild.depth;
        }
       
	}
	
public int bestAttribute(Node s){
		
		//calculate parent Node entropy
		labelCount labelCnt = getclassLabels(s);
		double entropy_parent = getNodeEntropy(labelCnt);
		double[] IG = new double[10];
		int temp = 0;
		
		//iterate and calculate IG for all attributes to decided Best attribute		
		for(int i=0;i<s.attributes.length-1;i++)
		{
			if(s.attributes[i] != "0")					 			//skip used attributes
			{
				temp = i;
				//Left Node
				labelCount labelCnt_left = getclassLabels(s, i, "0");
				double entropy_left = getNodeEntropy(labelCnt_left);
				
				//Right Node
				labelCount labelCnt_right = getclassLabels(s, i, "1");
				double entropy_right = getNodeEntropy(labelCnt_right);
				
				double totalInstances = labelCnt_left.count_Total + labelCnt_right.count_Total;
				
				//Information gain = parent Entropy- Weighted avg of children
				IG[i] = entropy_parent - ((labelCnt_left.count_Total/totalInstances) *  entropy_left + (labelCnt_right.count_Total/totalInstances) * entropy_right);
			}
				
		}
		
		double max = IG[temp];
		int maxIndex = temp;
		for(int i=0;i<s.attributes.length-1;i++)
		{
			if(s.attributes[i] != "0") //skip used attributes
			{
				if(IG[i] > max)
				{
					max = IG[i];
					maxIndex = i;
				}
			}
		}
		return maxIndex;
	}

public int randomAttribute(Node s){
	
	int randomNum=0;
	Random rn = new Random();
	int n = s.attributes.length-1;
	randomNum = rn.nextInt() % n;
	while(randomNum < 0 && randomNum > n)
	{
		randomNum = rn.nextInt() % n;
	}
	randomNum = Math.abs(randomNum);
	return randomNum;
}


	public String getClassLabel_leaf(Node s){
		labelCount labelCnt = getclassLabels(s);
		
		if(labelCnt.count_Positive >= labelCnt.count_Negative)
		{
			return "1";
		}else {
			return "0";  
		}
	}
	
	public void splitNode(Node s, int attributeIndex, Tree tree){
		
		s.splitAttribute = attributeIndex;
		Node left_node = new Node();
		Node right_node = new Node();
		left_node.number = tree.count;
		tree.count += 1;
		right_node.number = tree.count;
		tree.count += 1;
		
		for(int i=0; i<s.data.size(); i++)
		{
			if(s.data.get(i)[attributeIndex].equals("0"))
			{
				left_node.data.add(s.data.get(i));
			}
			else if(s.data.get(i)[attributeIndex].equals("1"))
			{
				right_node.data.add(s.data.get(i));
			}
		}
		
		//Removing already used attribute from the array
		String[] tempAttribArray = new String[20];
		tempAttribArray = Arrays.copyOf(s.attributes, s.attributes.length);
		tempAttribArray[attributeIndex] = "0";
		left_node.attributes = tempAttribArray;
		right_node.attributes = tempAttribArray;
		
		s.leftChild = left_node;
		s.rightChild = right_node;
		
		s.leftChild.splittedOn_attribute = attributeIndex;
		s.leftChild.splittedOn_value = 0;
		s.rightChild.splittedOn_attribute = attributeIndex;
		s.rightChild.splittedOn_value = 1;	
		s.leftChild.depth = s.depth + 1;
		s.rightChild.depth = s.depth + 1;
	}
	
	public boolean isPure(Node s){
		if(s.data != null)
		{
			int classLabelIndex = s.data.get(0).length-1;
			String firstLabelValue = s.data.get(0)[classLabelIndex];
			boolean testFlag = true;
			
			for(int i=1;i<s.data.size();i++)
			{
				if(s.data.get(i)[classLabelIndex].equals(firstLabelValue))
				{
					testFlag = true;
				}
				else {
					testFlag = false;
					break;
				}
			}
			
			if(testFlag == true)
			{
				return true;
			}
			else {
				return false;
			}
		}
		else {
			return true;
		}
	}
	
	public boolean isAttributeAvailable(Node s)
	{
		int flag = 0;
		for(int i=0;i<s.attributes.length-1;i++)
		{
			if(!s.attributes[i].equals("0"))
			{
				flag = 1;
			}
		}
		
		if(flag == 1)
			return true;
		
		return false;
	}
	
		
	public double getNodeEntropy(labelCount labelCnt){

		double total = (labelCnt.count_Negative + labelCnt.count_Positive);
		double Pi_Positive = ((double)labelCnt.count_Positive / total);
		double Pi_Negative = ((double)labelCnt.count_Negative / total);

		double positive_log;
		double negative_log;
		
		if(Pi_Positive!=0)    						 			//Assuming log2(0) = 0 as validation
		positive_log = Math.log(Pi_Positive)/ Math.log(2);
		else
			positive_log = 0;
		
		if(Pi_Negative!=0)
			negative_log = Math.log(Pi_Negative)/Math.log(2);
			else
				negative_log = 0;

		double nodeEntropy = - (Pi_Positive * (positive_log)) - (Pi_Negative * (negative_log));
		return nodeEntropy;
	}
	
	public labelCount getclassLabels(Node s, int attributeIndex, String label){
		labelCount labelCnt = new labelCount();
		int classIndex = s.data.get(0).length - 1;

		for(int i=0;i<s.data.size();i++)
        {
			if(s.data.get(i)[attributeIndex].equals(label))
			{				
        		 if(s.data.get(i)[classIndex].equals("0")) 		// filtering dataset on i th attribute.
        		 {
        			 labelCnt.count_Negative += 1;
        		 }
        		 else if(s.data.get(i)[classIndex].equals("1"))
        		 {
        			 labelCnt.count_Positive += 1;
        		 }
			}        		 
        }
		labelCnt.count_Total = labelCnt.count_Positive + labelCnt.count_Negative;
		return labelCnt;
	}
	
	public labelCount getclassLabels(Node s){
		
		labelCount labelCnt = new labelCount();
		int classIndex = s.data.get(0).length - 1;

		for(int i=0;i<s.data.size();i++)
        {
        		 if(s.data.get(i)[classIndex].equals("0"))
        		 {
        			 labelCnt.count_Negative += 1;
        		 }
        		 else if(s.data.get(i)[classIndex].equals("1"))
        		 {
        			 labelCnt.count_Positive += 1;
        		 }        		 
        }
		return labelCnt;
	}
	
	public double getAccuracy(ArrayList<String[]> data, Node root)
	{
		//Node temp = root;
		int totalInstances = data.size();
		double correctInstances = 0;
		
		for(int i=0;i< data.size();i++)
		{
			Node temp = new Node(root);
			while(temp.isLeaf == false)  									//Traverse till leaf
			{
				if(data.get(i)[temp.splitAttribute].equals("0"))
				{
					if(temp.leftChild != null)
					temp=temp.leftChild;
					else{
						break;
					}
				}
				else {
					if(temp.rightChild != null)
					temp=temp.rightChild;
					else{
						break;
					}
				}
			}
			//System.out.println(i + "=" + temp.classLabel);
				if(temp.classLabel.equals(data.get(i)[data.get(i).length-1]))
				{
					correctInstances += 1;
					//System.out.println(correctInstances);
				}
				
				
		}	
		return ((correctInstances * 100) / totalInstances );
	}
	
	public int getRandom(Tree tree)
	{
		int randomNum=0;
		Random rn = new Random();
		int n = tree.index;
		
		randomNum = rn.nextInt() % n;
		while(randomNum < 0 && randomNum > tree.index)
		{
			randomNum = rn.nextInt() % n;
		}
		randomNum = Math.abs(randomNum);
		return randomNum;
		
	}
	
	public void printTree(Node root, Tree tree)
	{
		printNode(root.leftChild,"", tree);
		printNode(root.rightChild,"", tree);
	}
	
	private void printNode(Node s, String str, Tree tree) {
		               if (s == null)
		                       return;
		               System.out.println();
		               System.out.print(str + tree.allAttributes[s.splittedOn_attribute] + " = " + s.splittedOn_value);
		               if (s.leftChild == null && s.rightChild == null) {
		                       System.out.print(" : "+ s.classLabel);
		               } else {
		            	   printNode(s.leftChild, str + "| ", tree);
		            	   printNode(s.rightChild, str + "| ", tree);
		               }
		       }
}

public class ID3vsRandom {
	
	public static void main(final String[] args) {
	    
	    try{
	    	
	    	//TRAINING DATA 
	    	File file = new File(args[0]);
			FileReader fileReader = new FileReader(file);
			BufferedReader bufferedReader = new BufferedReader(fileReader);
			String str = bufferedReader.readLine();

			String[] splitted = str.split("	");					
            splitted[splitted.length-1] = null;    //Attributes array
            
            String currentLine;
            String[] row = new String[10];
            ArrayList<String[]> trainingData = new ArrayList<String[]>();
            
            while((currentLine = bufferedReader.readLine()) != null)
            {
            	row = currentLine.split("	");
            	if(!currentLine.isEmpty())
            		trainingData.add(row);    					//training Data
            }
            
          //TEST DATA
            file = new File(args[1]);
            fileReader.close();
			fileReader = new FileReader(file);
			bufferedReader = new BufferedReader(fileReader);
			currentLine = bufferedReader.readLine(); //skip attribute line
			ArrayList<String[]> testData = new ArrayList<String[]>();       
            while((currentLine = bufferedReader.readLine()) != null)
            {
            	row = currentLine.split("	");
            	if(!currentLine.isEmpty())
            		testData.add(row);    //head data
            }
            
            
           // ID3 tree
            Tree treeID3 = new Tree();  					//create ID3 tree
            Node rootID3 = new Node(); 					//create root
            
            rootID3.number = treeID3.count;
            treeID3.count += 1;
            
            rootID3.attributes = splitted; 				//root node attributes
            treeID3.allAttributes = splitted;
            
            rootID3.data = trainingData;
            rootID3.depth = 0;
            
            treeID3.id3(rootID3,treeID3);    					//Constructed optimal tree using ID3
            double training_accuracy = treeID3.getAccuracy(testData,rootID3); //Accuracy
            
            System.out.println("\nDecision Tree Using ID3:");       
            treeID3.printTree(rootID3,treeID3);					//Print tree 
            System.out.println("\n\nNumber of training Instances = " + trainingData.size() + "\nNumber of training attributes = " + (rootID3.attributes.length-1) + "\nTotal number of nodes in the tree = " 
            		+ treeID3.count + "\nNumber of leaf nodes in the tree = " + treeID3.index+ "\nAccuracy of the model on the test dataset = " + training_accuracy + " %");
            
            // Tree using Random attribute selection
            Tree treeRandom = new Tree();  					
            Node rootRandom = new Node();
            
            rootRandom.number = treeRandom.count;
            treeRandom.count += 1;
            
            rootRandom.attributes = splitted;
            treeRandom.allAttributes = splitted;
            
            rootRandom.data = trainingData;
            rootRandom.depth = 0;     
            treeRandom.randomSelection(rootRandom, treeRandom);		//Constructed optimal tree selecting Random attributes
            double training_accuracy1 = treeRandom.getAccuracy(testData,rootRandom);
            System.out.println("\nDecision Tree Using Random attribute Selection:");
            treeRandom.printTree(rootRandom,treeRandom);			//Print tree   
            System.out.println("\n\nNumber of training Instances = " + trainingData.size() + "\nNumber of training attributes = " + (rootRandom.attributes.length-1) + "\nTotal number of nodes in the tree = " 
            		+ treeRandom.count + "\nNumber of leaf nodes in the tree = " + treeRandom.index+ "\nAccuracy of the model on the test dataset = " + training_accuracy1 + " %");
            
            double depthRandomTree, depthID3Tree;
            depthRandomTree = (double)treeRandom.sumLeaf/ treeRandom.index;
            depthID3Tree = (double)treeID3.sumLeaf / treeID3.index;
            System.out.println("----------------------------------------\nOUTPUT\n\t\t\tAvg depth\t Number of Nodes");
            System.out.println("Using ID3 :\t\t" + depthID3Tree + "\t" + treeID3.index);
            System.out.println("Using Random : \t\t" + depthRandomTree + "\t" + treeRandom.index);
                
			fileReader.close();		
			
	    }catch (IOException e)
	    {
	    	e.printStackTrace();
	    }
	  }

}
