package test.codegen;



/**
 * Node Interface Implementation:
 * 
 * 	- Create the following instance Variable:
 * 	  // Map from Key:Nt -> Value:(RuleEnum: rule, int: cost)
 * 	  private EnumMap<NT, MapEntry> table = new EnumMap<NT, MapEntry>(NT.class);
 * 
 * 	- The is(), put(), get(), cost(), and rule() methods have the following implementations:
 * 		is():
		public boolean is(NT nt) {
			return this.table.containsKey(nt);
		}
 * 
 * 		put():
		public MapEntry put(NT nt, MapEntry entry) {
			return this.table.put(nt, entry);
		}
 * 
 * 		get():
		public MapEntry get(NT nt) {
			return this.table.get(nt);
		}
 * 
 * 		cost():
		public int cost(NT nt) {
			MapEntry e = this.table.get(nt);
			return (e != null) ? e.cost : Integer.MAX_VALUE;
		}
 * 
 * 		rule():
		public RuleEnum rule(NT nt) {
			return (this.table.get(nt)).rule;
		}
 */
public interface Node {
	
public Node child1 ();
	
public Node child2 ();
	
public NodeKind kind ();
	
public boolean is (NT nt);
	
public MapEntry put (NT nt, MapEntry entry);
	
public MapEntry get (NT nt);
	
public int cost (NT nt);
	
public RuleEnum rule (NT nt);
	
public Node link ();

} // END INTERFACE Node
