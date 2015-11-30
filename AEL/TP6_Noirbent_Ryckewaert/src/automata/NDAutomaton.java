package automata;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * 
 * Implémentation d'un automate non déterministe. Version à compléter ou à
 * étendre.
 * 
 * @author Bruno.Bogaert (at) univ-lille1.fr
 *
 */
public class NDAutomaton extends AbstractAutomaton implements Recognizer, AutomatonBuilder {

	protected Set<State> initialStates;
	protected HashMap<Key, Set<State>> delta;

	public NDAutomaton() {
		super();
		initialStates = new PrintSet<State>();
		delta = new HashMap<Key, Set<State>>();
	}

	public boolean isInitial(String name) throws StateException {
		return isInitial(states.get(name));
	}

	public boolean isInitial(Integer id) throws StateException {
		return isInitial(states.get(id));
	}

	public Set<State> getTransitionSet(State from, char letter) {
		Set<State> s = delta.get(new Key(from, letter));
		if (s == null)
			return Collections.emptySet();
		else
			return Collections.unmodifiableSet(s);
	}

	public Set<State> getTransitionSet(String from, char letter) {
		return getTransitionSet(states.get(from), letter);
	}

	public Set<State> getTransitionSet(Integer from, char letter) {
		return getTransitionSet(states.get(from), letter);
	}

	public Set<State> getTransitionSet(Set<State> from, char letter) {
		Set<State> s = new HashSet<State>();
		for(State state : from){
			s.addAll(delta.get(new Key(state, letter)));
		}
		return s;
	}

	public Set<State> getInitialStates() {
		return Collections.unmodifiableSet(this.initialStates);
	}

	@Override
	public void setInitial(State s) {
		initialStates.add(s);
	}

	@Override
	public boolean isInitial(State s) {
		return initialStates.contains(s);
	}

	@Override
	public void addTransition(State from, Character letter, State to) {
		alphabet.add(letter);
		Key k = new Key(from, letter);
		Set<State> arrival = delta.get(k);
		if (arrival == null) {
			arrival = new PrintSet<State>();
		}
		if (!arrival.contains(to)) {
			arrival.add(to);
			delta.put(k, arrival);
		}
	}

	@Override
	public boolean accept(String word) {
		return this.accept(word, this.getInitialStates());
	}

	public boolean accept(String word, Set<State> from) {
		if (word.isEmpty()) {
			for (State state : from) {
				if (this.isAccepting(state))
					return true;
			}
			return false;
		} else {
			Set<State> s;
			s = getTransitionSet(from, word.charAt(0));
			return accept(word.substring(1), s);
		}
	}

	public Writer writeGraphvizTransitions(Writer buff) {
		PrintWriter out = new PrintWriter(buff);
		for (Map.Entry<Key, Set<State>> entry : delta.entrySet()) {
			for (State dest : entry.getValue()) {
				out.print("  " + entry.getKey().from.getId() + " -> " + dest.getId());
				out.println(" [label = \"" + entry.getKey().letter + "\" ]");
			}
		}
		return buff;
	}

	public Writer writeGraphvizInner(Writer buff) {
		writeGraphvizStates(buff, true);
		writeGraphvizInitials(buff);
		writeGraphvizTransitions(buff);
		return buff;
	}

	/**
	 * Returns true if the Set contains an accepting State
	 * 
	 * @param states
	 *            the states to check
	 * @return true if the Set contains an accepting State
	 */
	public boolean isAccepting(Set<State> states) {
		for (State s : states) {
			if (this.isAccepting(s))
				return true;
		}
		return false;
	}

	public String getName(Set<State> states) {
		String result;
		result = "|";
		for (State s : states)
			result += s.getName() + "|";
		return result;
	}

	/**
	 * Create a deterministic automaton corresponding to the current automaton
	 * 
	 * @return a deterministic Automaton
	 */
	public NDAutomaton deterministic() {
		NDAutomaton auto = new NDAutomaton();
		State state = auto.addNewState(this.getName(this.getInitialStates()));
		auto.setInitial(state);
		Map<Set<State>, State> correspondances = new HashMap<Set<State>, State>();
		correspondances.put(this.getInitialStates(), state);
		this.createDeterministic(correspondances, auto, this.getInitialStates());
		return auto;
	}

	/**
	 * Create a deterministic automaton
	 * 
	 * @param correspondances
	 *            what does each Set of State correspond to
	 * @param auto
	 *            l'automaton we want to create
	 * @param states
	 *            state in which we currently are
	 */
	public void createDeterministic(Map<Set<State>, State> correspondances, NDAutomaton auto, Set<State> states) {
		for (Character c : this.usedAlphabet()) {
			Set<State> setTo = this.getTransitionSet(states, c);
			State from = correspondances.get(states);
			State stateTo;
			if (correspondances.containsKey(setTo)) {
				stateTo = correspondances.get(setTo);
				auto.addTransition(from, c, stateTo);
			} else {
				stateTo = auto.addNewState(this.getName(states));
				if (this.isAccepting(setTo))
					auto.setAccepting(stateTo);
				correspondances.put(setTo, stateTo);
				auto.addTransition(from, c, stateTo);
				createDeterministic(correspondances, auto, setTo);
			}
		}
	}
}
