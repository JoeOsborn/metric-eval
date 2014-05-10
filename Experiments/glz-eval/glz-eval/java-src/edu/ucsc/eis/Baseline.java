package edu.ucsc.eis;

import java.util.Map;

/**
 * The Baseline class contains static methods that calculate distances between
 * pairs of input.
 */
public class Baseline {

	//private static final Logger log = Logger.getLogger(Baseline.class.getName());

	/**
	 * Calculates the Manhattan distance between two n-grams.
	 * 
	 * @param a
	 *            The first n-gram.
	 * @param b
	 *            The second n-gram.
	 * @return The Manhattan distance n-grams a and b
	 */
	public static float ngramDistance(Map<String, Integer> a,
			Map<String, Integer> b) {

		float distance = 0.0f;
		float sum = 0.0f;

		// log.log(Level.INFO, "Starting #ngramDistance between a and b.");

		// 3 cases, in a U b, b intersection a, a intersection b
		for (String key : a.keySet()) {
			float value = (float) a.get(key);
			if (a.containsKey(key) && b.containsKey(key)) {
				// a U b: a and b both have the key
				distance += (Math.abs(value - (float) b.get(key)));
			} else {
				// a intersect b: key in a and not in b
				distance += value;
			}
			sum += value;
		}
		for (String key : b.keySet()) {
			float value = (float) b.get(key);
			// b intersect a: key in b and not in a
			if (!a.containsKey(key) && b.containsKey(key)) {
				distance += value;
			}
			sum += value;
		}
		//log.log(Level.INFO, "distance: " + distance + " sum: " + sum);
		return distance/sum;
	}

	/**
	 * Determines the angular dissimilarity (pardon the misnomer in the method
	 * name) between two Maps, a and b.
	 * 
	 * @param a
	 *            First map.
	 * @param b
	 *            Second map.
	 * @return Angular distance between a and b with values of [0..1]
	 */
	public static float intentCosineDissimilarity(Map<String, Integer> a,
			Map<String, Integer> b) {

		float distance = 0.0f;
		float similarity = 0.0f;
		float dotProduct = 0.0f;
		float magA = 0;
		float magB = 0;

		// 3 cases, in a U b, b intersection a, a intersection b
		for (String key : a.keySet()) {
			float value = (float) a.get(key);
			if (a.containsKey(key) && b.containsKey(key)) {
				// a U b: a and b both have the key
				dotProduct += value * (float) b.get(key);
			}
			// update magnitude
			magA += Math.abs(value * value);
		}
		for (String key : b.keySet()) {
			float value = (float) b.get(key);
			// b intersect a: key in b and not in a
			magB += Math.abs(value * value);
		}
		similarity = (float) (dotProduct / (Math.sqrt(magA) * Math.sqrt(magB)));

		// angular dissimilarity
		distance = (float) (Math.acos(similarity) / Math.PI);

		// not returning directly for logging and normalization
		return distance;
	}

	/**
	 * Determines the angular dissimilarity (pardon the misnomer in the method
	 * name) between two arrays of ints, a and b.
	 * 
	 * @param a
	 *            First array.
	 * @param b
	 *            Second array.
	 * @return Angular distance between a and b with values of [0..1]
	 */
	public static float stateCosineDissimilarity(int[] a, int[] b) {

		float distance = 0.0f;
		float similarity = 0.0f;
		float dotProduct = 0.0f;
		int magA = 0;
		int magB = 0;

		if (a.length != b.length) {
			//log.log(Level.SEVERE, "arrays a and b are not the same length!");
			throw new Error("arrays a and b are not the same length!");
		}

		for (int i = 0; i < a.length; ++i) {
			dotProduct += (float) (a[i] * b[i]);
			magA += a[i] * a[i];
			magB += b[i] * b[i];
		}

		similarity = (float) (dotProduct / (Math.sqrt(magA) * Math.sqrt(magB)));

		// angular dissimilarity
		distance = (float) (Math.acos(similarity) / Math.PI);

		// not returning directly for logging and normalization
		return distance;
	}

}
