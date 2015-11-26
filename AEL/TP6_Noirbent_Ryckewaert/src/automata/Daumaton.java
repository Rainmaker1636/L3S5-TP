package automata;

import java.util.HashMap;

public class Daumaton extends NDAutomaton implements DeterministicAutomaton {

	protected HashMap<Key, State> delta;

	/**
	 * état initial de l'automate
	 * 
	 * @return état initial de l'automate
	 */
	public State getInitialState() {
		State s = null;
		for (State initialS : this.initialStates) {
			s = initialS;
		}
		return s;
	}

	/**
	 * transition pour s,letter. null si indéfinie
	 * 
	 * @param s
	 * @param letter
	 * @return transition delta(s,letter), null si indéfinie
	 * @throws StateException
	 *             si s est invalide
	 */
	public State getTransition(State from, char letter) throws StateException {
		State s = delta.get(new Key(from, letter));
		if (s == null)
			throw new StateException();
		else
			return s;
	}

	/**
	 * transition pour s,letter. null si indéfinie
	 * 
	 * @param name
	 *            nom d'un état s de l'automate
	 * @param letter
	 * @return transition delta(s,letter), null si indéfinie
	 * @throws StateException
	 */
	public State getTransition(String from, char letter) throws StateException {
		return getTransition(states.get(from), letter);
	}

	/**
	 * transition pour s,letter. null si indéfinie
	 * 
	 * @param id
	 *            : rang d'un état s de l'automate
	 * @param letter
	 * @return transition delta(s,letter), null si indéfinie
	 * @throws StateException
	 */
	public State getTransition(Integer id, char letter) throws StateException {
		return getTransition(states.get(id), letter);
	}

	@Override
	public void setInitial(automata.State s) {
		this.initialStates.clear();
		this.initialStates.add(s);
	}

	@Override
	public void addTransition(automata.State from, Character letter, automata.State to) {
		this.delta.put(new Key(from, letter), to);
	}
}
