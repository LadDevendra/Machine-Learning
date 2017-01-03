package assignment3;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class NaiveB {

	private static int instances = 0;

	// classification labels
	final private static int class_labels = 2;

	private static HashMap<Integer, Double> Prob_cls_labels = new HashMap<Integer, Double>();

	static ArrayList<Integer> cls_labels = new ArrayList<Integer>();
	static ArrayList<Integer> predictions = new ArrayList<Integer>();

	static List<Map<Tuple, Integer>> tuples = new ArrayList<Map<Tuple, Integer>>();
	static List<Map<Tuple, Double>> cond_probList = new ArrayList<Map<Tuple, Double>>();

	static HashMap<Integer, String> col_names = new HashMap<Integer, String>();
	static HashMap<Integer, Integer> label_count = new HashMap<Integer, Integer>();

	// Main method execution
	public static void main(String[] args) throws IOException {
		// passing training and testing data through command line args
		String train_data = args[0];
		String test_data = args[1];

		usingTrainingData(train_data);
		classLabelProbs();
		classPrediction(train_data);
		estimatedParameters();

		System.out.format("Accuracy on train dataset : %.3f\n", comp_Accuracy());

		// clearing for test prediction
		instances = 0;
		
		// clearing array lists for test prediction 
		cls_labels.clear();
		predictions.clear();

		classPrediction(test_data);
		System.out.format("Accuracy on test dataset : %.3f\n", comp_Accuracy());
	}

	// attribute value and corresponding class label
	public static class Tuple {
		int attr_value;
		int corres_cls_label;

		Tuple(int i1, int i2) {
			attr_value = i1;
			corres_cls_label = i2;
		}

		public int hashCode() {
			int hash = 3;
			hash = hash * 53 + attr_value + corres_cls_label;
			return hash;
		}

		public boolean equals(Object o) {
			final Tuple other = (Tuple) o;
			if (other.attr_value == attr_value
					&& other.corres_cls_label == corres_cls_label)
				return true;
			else
				return false;
		}

	}

	// building data structure using training dataset
	private static void usingTrainingData(String train)
			throws FileNotFoundException, IOException {
		BufferedReader br = new BufferedReader(new FileReader(train));

		String line = br.readLine();
		String[] columns = line.split("\\s+");

		// First line is column names
		for (int i = 0; i < columns.length - 1; ++i) {
			// Storing the column names
			col_names.put(i, columns[i]);

			Map<Tuple, Integer> init1 = new HashMap<Tuple, Integer>();
			Map<Tuple, Double> init2 = new HashMap<Tuple, Double>();

			// Initializing tuples and cond_probList
			tuples.add(init1);
			cond_probList.add(init2);
		}

		while ((line = br.readLine()) != null) {
			columns = line.split("\\s+");
			int ds_cls_labels = Integer.parseInt(columns[columns.length - 1]);
			for (int i = 0; i < columns.length - 1; ++i) {
				int attr = Integer.parseInt(columns[i]);

				Map<Tuple, Integer> tuple_value = tuples.get(i);
				Tuple tuple = new Tuple(attr, ds_cls_labels);

				// counting the label's of all attribute
				/*
				 * else loop is executed at first
				 */
				if (tuple_value.containsKey(tuple)) {
					tuples.get(i).put(tuple, tuple_value.get(tuple) + 1);
				} else {
					tuples.get(i).put(tuple, 1);
				}
			}

			// total number of zeros and ones in the Class label column
			/*
			 * else loop is executed at first
			 */
			if (label_count.containsKey(ds_cls_labels)) {
				label_count.put(ds_cls_labels,
						label_count.get(ds_cls_labels) + 1);
			} else {
				label_count.put(ds_cls_labels, 1);
			}
			instances++;
		}		
		
		// Computing the conditional probability table for prediction.
		for (int i = 0; i < tuples.size(); ++i) {
			Map<Tuple, Integer> tuple_value = tuples.get(i);
			for (Tuple t : tuple_value.keySet()) {
				
				double prob = tuple_value.get(t)
						/ (double) label_count.get(t.corres_cls_label);

				cond_probList.get(i).put(t, prob);
			}
		}
		br.close();
	}

	// Computing classification labels probabilities
	private static void classLabelProbs() {
		for (int cls = 0; cls < class_labels; ++cls) {
			double prob = (double) label_count.get(cls) / instances;
			Prob_cls_labels.put(cls, prob);
			System.out.println("Number of " + cls + " classification labels = "
					+ label_count.get(cls));
		}

		// Resetting to zero as the variable is used in other methods as well
		instances = 0;
	}	

	// Making prediction for the data using the probability table 
	public static void classPrediction(String instance) throws IOException {
		String line = "";
		BufferedReader br = new BufferedReader(new FileReader(instance));

		// ignore column names
		br.readLine();

		while ((line = br.readLine()) != null) {
			String[] columns = line.split("\\s+");
			double[] probs = new double[class_labels];

			for (int cls = 0; cls < class_labels; ++cls) {
				probs[cls] = Prob_cls_labels.get(cls);
				for (int i = 0; i < columns.length - 1; ++i) {
					Tuple t = new Tuple(Integer.parseInt(columns[i]), cls);

					/* computing joint probability using cond_probabilities of
					   attributes for the cls_labels 0 and 1*/
					Map<Tuple, Double> m = cond_probList.get(i);
					probs[cls] *= m.get(t);
				}
			}

			// Selecting max probability as the prediction
			predictions.add(SelectMaxProbClass(probs));

			int cls_label = Integer.parseInt(columns[columns.length - 1]);
			// In order to calculate prediction vs actual accuracy
			cls_labels.add(cls_label);
			instances++;
		}
		br.close();
	}
	
	// Estimated conditional probabilities
	private static void estimatedParameters() {
		System.out.println();
		System.out.println("The estimated parameters are :");
		for (int cls = 0; cls < class_labels; ++cls) {
			// for loop on the number of class labels possible
			String probs_disp = "P(class=" + cls + ")="
					+ Double.toString(Prob_cls_labels.get(cls)) + " ";
			for (int attr = 0; attr < col_names.size(); ++attr) {
				for (int attrVal = 0; attrVal < 2; ++attrVal) {
					Tuple t = new Tuple(attrVal, cls);
					Map<Tuple, Double> m1 = cond_probList.get(attr);
					String prob = Double.toString(m1.get(t));

					probs_disp += "P(" + col_names.get(attr) + "=" + attrVal
							+ "|class=" + cls + ")=" + prob.substring(0, 5)
							+ " ";
				}
			}
			System.out.println(probs_disp);
		}
		System.out.println();
	}

	// Returns index of the max probability
	private static int SelectMaxProbClass(double[] prob) {

		int idx = -1;
		double value = Double.MIN_NORMAL;

		for (int i = 0; i < prob.length; ++i) {
			if (prob[i] > value) {
				value = prob[i];
				idx = i;
			}
		}
		return idx;
	}

	// computing accuracy -> prediction vs actual values
	private static double comp_Accuracy() {
		int num_errors = 0;

		for (int i = 0; i < instances; ++i) {
			int given_cls_labels = cls_labels.get(i);
			int pred_cls_labels = predictions.get(i);
			if (Math.abs(given_cls_labels - pred_cls_labels) > 0) {
				num_errors++;
			}
		}

		// accuracy
		double acc = (double) (instances - num_errors) / instances;
		return acc * 100;
	}

}
