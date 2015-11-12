package automata;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.Collections;
import java.util.HashMap;
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

	@Override
	public boolean isInitial(String name) throws StateException {
		return isInitial(states.get(name));
	}

	@Override
	public boolean isInitial(Integer id) throws StateException {
		return isInitial(states.get(id));
	}

	@Override
	public Set<State> getTransitionSet(State from, char letter) {
		Set<State> s = delta.get(new Key(from, letter));
		if (s == null)
			return Collections.emptySet();
		else
			return Collections.unmodifiableSet(s);
	}

	@Override
	public Set<State> getTransitionSet(String from, char letter) {
		return getTransitionSet(states.get(from), letter);
	}

	@Override
	public Set<State> getTransitionSet(Integer from, char letter) {
		return getTransitionSet(states.get(from), letter);
	}

	public Set<State> getTransitionSet(Set<State> from, char letter) {
		Set<State> s = delta.get(new Key(from, letter));
		if (s == null)
			return Collections.emptySet();
		else
			return Collections.unmodifiableSet(s);
	}
	
	@Override
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
		// TODO : implémenter correctement la méthode accept
		return false;
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

}
