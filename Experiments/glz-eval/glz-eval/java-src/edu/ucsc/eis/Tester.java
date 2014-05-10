package edu.ucsc.eis;

import java.util.HashMap;
import java.util.Map;

public class Tester {

	private static final Map<String, Integer> mapA = new HashMap<String, Integer>();
	private static final Map<String, Integer> mapB = new HashMap<String, Integer>();;
	private static final int[] arrayA = {75, 0, 75, 100, 0, 100, 15, 20, 40, 0, 0, 100};
	private static final int[] arrayB = {50, 50, 70, 100, 100, 100, 70, 20, 40, 0, 0, 100};
	
	
	static { 
		mapA.put("idolize", 1);
		mapA.put("askout", 2);
		mapA.put("woo", 4);
		
		mapB.put("askout", 1);
		mapB.put("woo", 5);
		mapB.put("flirt", 1);
		mapB.put("annoy", 1);
		
	}
		
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		System.out.println("baseline1: " + Baseline.ngramDistance(mapA, mapB));
		System.out.println("baseline2: " + Baseline.intentCosineDissimilarity(mapA, mapB));
		System.out.println("baseline3: " + Baseline.stateCosineDissimilarity(arrayA, arrayB));
	}

}
