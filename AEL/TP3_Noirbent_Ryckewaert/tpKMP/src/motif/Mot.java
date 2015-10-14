package motif;

/**
 * Classe Mot : dispose de methodes de recherche de facteur. Classiquement, un
 * facteur est ici appele motif, pour reprendre le vocabulaire de ce genre
 * d'algorithmes. un Mot est forme d'une chaine de caracteres (String)
 *
 * @author Anne-Cecile Caron
 */

public class Mot {

	private String m;
	// le mot

	/**
	 * Constructeur de Mot
	 *
	 * @param m
	 *            La chaine de caracteres qui constitue le Mot
	 */
	public Mot(String m) {
		this.m = m;
	}

	/**
	 * Renvoie une representation du Mot sous forme d'une chaine
	 * 
	 * @return la chaine qui constitue le mot courant.
	 */
	public String toString() {
		return this.m;
	}

	/**
	 * Renvoie la longueur du mot
	 *
	 * @return le nombre de caracteres qui forment le mot
	 */
	public int length() {
		return this.m.length();
	}

	/**
	 * Teste si le mot courant est facteur du mot passe en parametre a partir de
	 * la position pos
	 *
	 * @param mot2
	 *            Le mot dans lequel on cherche le mot courant
	 * @param pos
	 *            La position dans mot2 a partir de laquelle on recherche le mot
	 *            courant
	 * @return true ssi le mot courant est facteur de mot2 a partir de pos
	 */
	public boolean estFacteurDe(Mot mot2, int pos) {
		for (int j = 0; j < this.length(); j++) {
			if (mot2.m.charAt(pos + j) != this.m.charAt(j)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Teste si le mot courant est prefixe du mot passe en parametre
	 *
	 * @param mot2
	 *            Le mot dans lequel on recherche le mot courant
	 * @return true ssi le mot courant est prefixe de mot2
	 */
	public boolean estPrefixeDe(Mot mot2) {
		return this.estFacteurDe(mot2, 0);
	}

	/**
	 * Teste si le mot courant est suffixe du mot passe en parametre
	 *
	 * @param mot2
	 *            Le mot dans lequel on recherche le mot courant
	 * @return true ssi le mot courant est suffixe de mot2
	 */
	public boolean estSuffixeDe(Mot mot2) {
		return this.estFacteurDe(mot2, mot2.m.length() - this.m.length());
	}

	/**
	 * Recherche d'un motif dans un mot avec l'algorithme naif.
	 *
	 * @param motif
	 *            Le motif a rechercher dans le mot this.
	 * @return L'indice ou on a trouve le motif, -1 si pas trouve.
	 */
	public int indiceMotifNaif(Mot motif) {
		int tailleMot = this.length();
		int tailleMotif = motif.length();
		for (int i = 0; i < tailleMot - tailleMotif; i++) {
			if (motif.estFacteurDe(this, i))
				return i;
		}
		return -1;
	}

	/**
	 * Recherche d'un motif dans un mot avec l'algorithme base sur la
	 * construction d'un automate. L'avantage est que l'on ne traite qu'une
	 * seule fois chaque lettre du mot dans lequel on recherche le motif.
	 *
	 * @param motif
	 *            Le motif a rechercher dans le mot this.
	 * @return L'indice ou on a trouve le motif, -1 si pas trouve.
	 */

	public int indiceMotifAutomate(Mot motif) {
		if (motif.length() == 0)
			return 0;
		else {
			AutomateMotif automate = new AutomateMotif(motif);
			int m = motif.length();
			int etat = automate.etatInitial();
			for (int i = 0; i < this.length(); i++) {
				etat = automate.transition(etat, this.m.charAt(i));
				if (etat == automate.etatFinal())
					return (i + 1 - m);
			}
		}
		return -1;
	}

	/**
	 * Recherche d'un motif dans un mot avec l'algorithme de Knuth-Morris-Pratt.
	 * L'avantage est que l'on n'a pas besoin de construire tout l'automate
	 *
	 * @param motif
	 *            Le motif a rechercher dans le mot this.
	 * @return L'indice ou on a trouve le motif, -1 si pas trouve.
	 */
	public int indiceMotifKMP(Mot motif) {
		if (motif.length() == 0)
			return 0;
		else {
			int[] pref = calculerFonctionPrefixe(motif);
			int q = 0;
			for (int i = 0; i < this.length(); i++) {
				while (q > 0 && motif.toString().charAt(q) != this.toString().charAt(i)) {
					q = pref[q];
				}
				if (motif.toString().charAt(q) == this.toString().charAt(i))
					q++;
				if (q == motif.length())
					return (i - motif.length() + 1);
			}
			return -1;
		}
	}

	private int[] calculerFonctionPrefixe(Mot motif) {
		int pref[] = new int[motif.length()];
		pref[0] = 0;
		int k = 0;
		for (int i = 1; i < motif.length(); i++) {
			while (k > 0 && motif.toString().charAt(k) != motif.toString().charAt(i)) {
				k = pref[k];
			}
			if (motif.toString().charAt(k) == motif.toString().charAt(i))
				k++;
			pref[i] = k;
		}
		return pref;
	}

	/**
	 * le programme principal permet de tester Knuth-Morris-Pratt On passe sur
	 * la ligne de commande comme premier argument, le mot, et comme second
	 * argument, le motif.
	 */
	public static void main(String args[]) {

		Mot m = new Mot("bonjour");
		Mot m1 = new Mot("aaabbabcaaabcabdabcdeaaabcddabcd");
		Mot m2 = new Mot("abcefghijklmnopqrstuvxyz");

		Mot p = new Mot("abcd");
		Mot p1 = new Mot("abc");
		Mot p2 = new Mot("abcde");
		Mot p3 = new Mot("jo");
		Mot p4 = new Mot("jour");
		Mot p5 = new Mot("bonjour");

		System.out.println("Test pour indice motif naif");

		System.out.println("\nLe mot est : " + m);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m.indiceMotifNaif(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m.indiceMotifNaif(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m.indiceMotifNaif(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m.indiceMotifNaif(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m.indiceMotifNaif(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m.indiceMotifNaif(p5));

		System.out.println("\nLe mot est : " + m1);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m1.indiceMotifNaif(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m1.indiceMotifNaif(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m1.indiceMotifNaif(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m1.indiceMotifNaif(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m1.indiceMotifNaif(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m1.indiceMotifNaif(p5));

		System.out.println("\nLe mot est : " + m2);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m2.indiceMotifNaif(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m2.indiceMotifNaif(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m2.indiceMotifNaif(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m2.indiceMotifNaif(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m2.indiceMotifNaif(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m2.indiceMotifNaif(p5));

		System.out.print("\n\n");

		System.out.println("Test pour indice motif automate\n");

		System.out.println("\nLe mot est : " + m);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m.indiceMotifAutomate(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m.indiceMotifAutomate(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m.indiceMotifAutomate(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m.indiceMotifAutomate(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m.indiceMotifAutomate(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m.indiceMotifAutomate(p5));

		System.out.println("\nLe mot est : " + m1);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m1.indiceMotifAutomate(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m1.indiceMotifAutomate(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m1.indiceMotifAutomate(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m1.indiceMotifAutomate(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m1.indiceMotifAutomate(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m1.indiceMotifAutomate(p5));

		System.out.println("\nLe mot est : " + m2);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m2.indiceMotifAutomate(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m2.indiceMotifAutomate(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m2.indiceMotifAutomate(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m2.indiceMotifAutomate(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m2.indiceMotifAutomate(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m2.indiceMotifAutomate(p5));

		System.out.print("\n");

		System.out.println("Test pour indice motif KMP\n");

		System.out.println("\nLe mot est : " + m);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m.indiceMotifKMP(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m.indiceMotifKMP(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m.indiceMotifKMP(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m.indiceMotifKMP(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m.indiceMotifKMP(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m.indiceMotifKMP(p5));

		System.out.println("\nLe mot est : " + m1);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m1.indiceMotifKMP(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m1.indiceMotifKMP(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m1.indiceMotifKMP(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m1.indiceMotifKMP(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m1.indiceMotifKMP(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m1.indiceMotifKMP(p5));

		System.out.println("\nLe mot est : " + m2);
		System.out.println("Le motif : " + p + " est présent à l'indice : " + m2.indiceMotifKMP(p));
		System.out.println("Le motif : " + p1 + " est présent à l'indice : " + m2.indiceMotifKMP(p1));
		System.out.println("Le motif : " + p2 + " est présent à l'indice : " + m2.indiceMotifKMP(p2));
		System.out.println("Le motif : " + p3 + " est présent à l'indice : " + m2.indiceMotifKMP(p3));
		System.out.println("Le motif : " + p4 + " est présent à l'indice : " + m2.indiceMotifKMP(p4));
		System.out.println("Le motif : " + p5 + " est présent à l'indice : " + m2.indiceMotifKMP(p5));

	}
}