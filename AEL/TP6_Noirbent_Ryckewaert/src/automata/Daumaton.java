package automata;

import java.util.Collections;
import java.util.HashMap;
import java.util.Set;

import automata.AbstractAutomaton.Key;

public class Daumaton extends AbstractAutomaton implements DeterministicAutomaton {
		State initialState;
		protected HashMap<Key,State> delta;

		/**
		 * état initial de l'automate
		 * 
		 * @return état initial de l'automate
		 */
		public State getInitialState(){
			return this.initialState;
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
		public State getTransition(State from, char letter) throws StateException{
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
		public State getTransition(String from, char letter) throws StateException{
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
		public State getTransition(Integer id, char letter) throws StateException{
			return getTransition(states.get(id), letter);
		}

		@Override
		public <State> getInitialState() {
			return this.initialState;
		}

		@Override
		public boolean isInitial(String name) throws StateException {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public boolean isInitial(Integer id) throws StateException {
			// TODO Auto-generated method stub
			return false;
		}
		@Override
		public void setInitial(automata.State s) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public boolean isInitial(automata.State s) {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public void addTransition(automata.State from, Character letter, automata.State to) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public boolean accept(String word) throws StateException {
			// TODO Auto-generated method stub
			return false;
		}

	}

}
