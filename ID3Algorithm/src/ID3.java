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
	   
	   public Node()
	   {
		   isLeaf = false;
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
	
	public int count;
	
	public Tree()
	{
		count = 0;
		index = 0;
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
        	}
        }
        else
        {
        	s.leftChild.isLeaf = true;
        	s.leftChild.classLabel = getClassLabel_leaf(s.leftChild);
        	tree.parentArray[tree.index] = s; 						//saving parent node reference
    		tree.childArray[tree.index] = s.leftChild.number; 		//Saving child node number
    		tree.index += 1;
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
        	}
        }
        else{
        	s.rightChild.isLeaf = true;
        	s.rightChild.classLabel = getClassLabel_leaf(s.rightChild);
        	tree.parentArray[tree.index] = s; 						//saving parent node reference
    		tree.childArray[tree.index] = s.rightChild.number; 		//Saving child node number
    		tree.index += 1;
        }
       
	}
	
	public String getClassLabel_leaf(Node s)
	{
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
		Node temp = root;
		int totalInstances = data.size();
		double correctInstances = 0;
		
		for(int i=0;i< data.size();i++)
		{
			temp = root;
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
				if(temp.classLabel.equals(data.get(i)[data.get(i).length-1]))
				{
					correctInstances += 1;
				}
		}	
		return ((correctInstances * 100) / totalInstances );
	}
	
	public boolean pruning(Tree tree, String pruningFactor) {
		double pFactor = Double.parseDouble(pruningFactor);
		int noNodes = (int)(pFactor * tree.count);
		
		int randomNum=0;
		int leafNumber;
		for(int i=0;i<noNodes;i++)
		{	
			randomNum = getRandom(tree);
			
			while(tree.childArray[randomNum] == 0)
			{
				randomNum = getRandom(tree);
			}
			leafNumber = tree.childArray[randomNum];
			
			if( tree.parentArray[randomNum].leftChild != null && tree.parentArray[randomNum].leftChild.number == leafNumber)
			{
				tree.parentArray[randomNum].leftChild = null; 												//pruned leaf node
				tree.parentArray[randomNum].classLabel = getClassLabel_leaf(tree.parentArray[randomNum]); 	//updated class label
			}
			else if(tree.parentArray[randomNum].rightChild != null && tree.parentArray[randomNum].rightChild.number == leafNumber)
			{
				tree.parentArray[randomNum].rightChild = null;
				tree.parentArray[randomNum].classLabel = getClassLabel_leaf(tree.parentArray[randomNum]);
			}
			tree.childArray[randomNum] = 0;		    
		}	
		return true;
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

public class ID3 {
	
	public static void main(final String[] args) {
	    
	    try{
	    	
	    	//TRAINING DATA 
	    	File file = new File("/Users/devendralad/Documents/workspace/ID3Algorithm/src/train.dat");
			FileReader fileReader = new FileReader(file);
			BufferedReader bufferedReader = new BufferedReader(fileReader);
			String str = bufferedReader.readLine();

			String[] splitted = str.split("	");					
            splitted[splitted.length-1] = null;
            
            String currentLine;
            Tree tree = new Tree();  					//create tree
            Node root = new Node(); 					//create root
            
            root.number = tree.count;
            tree.count += 1;
            
            root.attributes = splitted; 				//root node attributes
            tree.allAttributes = splitted;
            
            String[] row = new String[10];
            ArrayList<String[]> trainingData = new ArrayList<String[]>();
            
            while((currentLine = bufferedReader.readLine()) != null)
            {
            	row = currentLine.split("	");
            	if(!currentLine.isEmpty())
            	root.data.add(row);    					//root node data
            }
            
            trainingData = root.data;
            
            tree.id3(root,tree);    					//Constructed optimal tree using ID3
            
            System.out.println("\nDecision Tree :");
            tree.printTree(root,tree);					//Print tree 
            
            double training_accuracy = tree.getAccuracy(trainingData,root);
            System.out.println("\n\nOutput : \nPre-Pruned Accuracy\n---------------");
            System.out.println("Number of training Instances = " + trainingData.size() + "\nNumber of training attributes = " + (root.attributes.length-1) + "\nTotal number of nodes in the tree = " 
            		+ tree.count + "\nNumber of leaf nodes in the tree = " + tree.index + "\nAccuracy of the model on the training dataset = " + training_accuracy + " %");
            
            
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
            
            double testing_accuracy = tree.getAccuracy(testData,root);
            System.out.println("\nNumber of testing Instances = " + testData.size() + "\nNumber of testing attributes = " + (root.attributes.length-1) + "\nAccuracy of the model on the test dataset = " + testing_accuracy + " %");    
			fileReader.close();
			
			//PRUNING..
			double pFactor = Double.parseDouble(args[2] );
			int noNodes = (int)(pFactor * tree.count);			
			boolean success = tree.pruning(tree,args[2]);
			if(success)
			{
				 double training_accuracy_pruned = tree.getAccuracy(trainingData,root);				 
				 System.out.println("\n\nPost-Pruned Accuracy\n---------------");
		            System.out.println("Number of training Instances = " + trainingData.size() + "\nNumber of training attributes = " + (root.attributes.length-1) + "\nTotal number of nodes in the tree = " 
		            		+ (tree.count - noNodes) + "\nNumber of leaf nodes in the tree = " + (tree.index - noNodes) + "\nAccuracy of the model on the training dataset = " + training_accuracy_pruned + " %");
				 
				 double testing_accuracy_pruned = tree.getAccuracy(testData, root);
				 System.out.println("\nNumber of testing Instances = " + testData.size() + "\nNumber of testing attributes = " + (root.attributes.length-1) + "\nAccuracy of the model on the test dataset = " + testing_accuracy_pruned + " %");
			}
			else {
				System.out.println("Pruning Failed");
			}
			
	    }catch (IOException e)
	    {
	    	e.printStackTrace();
	    }
	  }

}
