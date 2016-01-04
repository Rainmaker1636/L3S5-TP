package automata;

import java.util.HashMap;

public class AhoCarosick extends Daumaton {

	protected HashMap<State, State> repli;

	public AhoCarosick(String[] u) {
		int nom = 0;
		int n = u.length;
		State[] finBranche = new State[n];
		int i, l;
		State racine = this.addNewState(Integer.toString(nom));
		nom++;
		for (i = 0; i < n; i++) {
			finBranche[i] = racine;
		}
		for (l = 0; l < this.maxLength(u); l++) {
			for (i = 0; i < n; i++) {
				if (l < u[i].length()) {
					Character letter = u[i].charAt(l);
					State q;
					q = this.getTransition(finBranche[i], letter);
					if (this.delta.containsKey(new Key(finBranche[i], letter))) {
						q = creerNouvelEtat(finBranche[i], letter);
						nom++;
					}
					finBranche[i] = q;
					if (l + 1 == u[i].length())
						this.setAccepting(finBranche[i]);
				}
			}
		}
	}

	public State creerNouvelEtat(State parent, Character lettre) {
		State q = this.addNewState();
		State s = this.addNewState();
		State racine = this.getInitialState();
		State e;
		this.addTransition(parent, lettre, q);

		if (parent == racine) {
			this.repli.put(q, racine);
		} else {
			s = parent;
			do {
				s = this.repli.get(s);
				e = this.getTransition(s, lettre);
			} while (e == null && s != racine);

			if (e != null) {
				this.repli.put(q, e);
				if (this.isAccepting(e)) {
					this.setAccepting(q);
				} else {
					this.repli.put(q, racine);
				}
			}
		}
		return q;
	}

	public void completerAutomate() {
		State racine = this.getInitialState();
		for (State q : this.getStates()) {
			for (Character lettre : this.alphabet) {
				if (this.getTransition(q, lettre) == null) {
					if (q == racine) {
						this.addTransition(q, lettre, racine);
					} else {
						this.addTransition(q, lettre, this.getTransition(this.repli.get(q), lettre));
					}
				}
			}
		}
	}

	public int maxLength(String[] mots) {
		int max;
		max = mots[0].length();
		for (int i = 1; i < mots.length; i++) {
			if (mots[i].length() > max)
				max = mots[i].length();
		}
		return max;
	}
}
