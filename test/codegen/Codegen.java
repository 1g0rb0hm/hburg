package test.codegen;

import static test.codegen.NT.*;
import static test.codegen.RuleEnum.*;
import test.codegen.NT;
import test.codegen.RuleEnum;
import test.codegen.MapEntry;
import java.util.EnumSet;
// @USER INCLUDES START

		import slcomp.parser.Obj;
		import java.util.LinkedList;
		import java.util.List;
	
// @USER INCLUDES END

// Codegen Class
public class Codegen {


// @USER CODE START

		/* -------- General Declerations and Cost functions if present */
			private static int nextreg  = 0;
	
			private static String nreg() {
				return "r" + ++nextreg;
			}

			private static String label(LirNode n) {
				return (n.label != null) ? n.label + ":" : "";
			}
			/* The following is our instruction list */
			public static List<:String:> l = new LinkedList<:String:>();
	
// @USER CODE END


/**
 * emit():
 *   Generate Code for AST starting with root node.
 */
public static void emit (Node n) {
	Codegen.Tiling.tile(n);
	Codegen.Eval.eval_root(n);
} // END METHOD emit()


// Tiling Class
private static class Tiling {


 private static EnumSet linkSet = EnumSet.of(ADD, C2C, C2I, CBR, CLOAD, CLOADA, CMP_EQ, CMP_GE, CMP_GT, CMP_LE, CMP_LT, CMP_NE, CSTORE, CSTOREA, DIV, FUN, FUNAP, FUNAPP, I2C, I2I, JUMP, JUMPI, LOAD, LOADA, LSHIFT, MOD, MULT, NOP, RETURN, RSHIFT, SIDEFFECT, SIDEFFECTP, STORE, STOREA, SUB, TBL);
 private static EnumSet arity0Set = EnumSet.of(LAB, NOP, NUM, VAL);
 private static EnumSet arity1Set = EnumSet.of(C2C, C2I, CLOAD, CLOADA, FUN, FUNAP, I2C, I2I, JUMP, JUMPI, LOAD, LOADA, RETURN, SIDEFFECT);
 private static EnumSet arity2Set = EnumSet.of(ADD, AND, CBR, CMP_EQ, CMP_GE, CMP_GT, CMP_LE, CMP_LT, CMP_NE, CSTORE, CSTOREA, DIV, ENTER, FUNAPP, LSHIFT, MOD, MULT, OR, RSHIFT, SIDEFFECTP, STORE, STOREA, SUB, TBL, XOR);

/**
 * label():
 *   Label each AST node appropriately.
 */
private static void label (Node n, NT nt, int c, RuleEnum r) {
	if (c < n.cost(nt)) {
		n.put(nt, new MapEntry(c, r));
		closure(n, nt, r);
	}
} // END METHOD label()

/**
 * tile():
 *    Tile the AST as in [Cooper p.566]
 */
public static void tile (Node n) {
	assert (n != null) : "ERROR - Can not tile null node.";
	if (arity0Set.contains(n.kind())) {
		label_0(n);
	} else if (arity1Set.contains(n.kind())) {
		tile(n.child1());
		label_1(n);
	} else if (arity2Set.contains(n.kind())) {
		tile(n.child1());
		tile(n.child2());
		label_2(n);
	} else {
		System.err.println("ERROR: Encountered undefined node: " + n.kind() );
	}

	if (linkSet.contains(n.kind())) {
		Node link = n.link();
		if (link != null) tile(link);
	}
} // END METHOD tile()

/**
 * label_0():
 *   Label nodes with arity 0
 */
private static void label_0 (Node n) {
	int cost;
	switch (n.kind()) {
		case LAB: {
			cost = 0;
			label(n, NT_LAB, cost, R_LAB_LAB_0);
			break;
		}
		case NOP: {
			cost = 1;
			label(n, NT_STMT, cost, R_STMT_NOP_1);
			break;
		}
		case NUM: {
			cost = 0;
			label(n, NT_CON, cost, R_CON_NUM_0);
			break;
		}
		case VAL: {
			cost = 0;
			label(n, NT_REG, cost, R_REG_VAL_55);
			break;
		}
		default: {
			throw new AssertionError("ERROR - label_0(): Unhandeled Node kind: " + n.kind());
		}
	} // END SWITCH
} // END METHOD label_0()

/**
 * label_1():
 *   Label nodes with arity 1
 */
private static void label_1 (Node n) {
	int cost;
	switch (n.kind()) {
		case C2C: {
			cost = n.child1().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_C2C_50);
			break;
		}
		case C2I: {
			cost = n.child1().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_C2I_49);
			break;
		}
		case CLOAD: {
			cost = n.child1().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_CLOAD_42);
			break;
		}
		case CLOADA: {
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_REG) && n.child1().child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_CLOADA_41);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_REG) && n.child1().child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_CLOADA_40);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_CON) && n.child1().child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_CLOADA_39);
			}
			break;
		}
		case FUN: {
			cost = n.child1().cost(NT_STMT) + 0;
			label(n, NT_STMT, cost, R_STMT_FUN_8);
			break;
		}
		case FUNAP: {
			cost = n.child1().cost(NT_JUMP) + 1;
			label(n, NT_REG, cost, R_REG_FUNAP_53);
			break;
		}
		case I2C: {
			cost = n.child1().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_I2C_48);
			break;
		}
		case I2I: {
			cost = n.child1().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_I2I_51);
			break;
		}
		case JUMP: {
			cost = n.child1().cost(NT_REG) + 1;
			label(n, NT_JUMP, cost, R_JUMP_JUMP_1);
			break;
		}
		case JUMPI: {
			cost = n.child1().cost(NT_LAB) + 1;
			label(n, NT_JUMP, cost, R_JUMP_JUMPI_0);
			break;
		}
		case LOAD: {
			cost = n.child1().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_LOAD_47);
			break;
		}
		case LOADA: {
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_REG) && n.child1().child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_LOADA_46);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_CON) && n.child1().child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_LOADA_45);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_REG) && n.child1().child2().is(NT_LAB)) {
				cost = n.child1().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_LOADA_44);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_LAB) && n.child1().child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_LOADA_43);
			}
			break;
		}
		case RETURN: {
			cost = n.child1().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_RETURN_0);
			break;
		}
		case SIDEFFECT: {
			cost = n.child1().cost(NT_JUMP) + 1;
			label(n, NT_STMT, cost, R_STMT_SIDEFFECT_7);
			break;
		}
		default: {
			throw new AssertionError("ERROR - label_1(): Unhandeled Node kind: " + n.kind());
		}
	} // END SWITCH
} // END METHOD label_1()

/**
 * label_2():
 *   Label nodes with arity 2
 */
private static void label_2 (Node n) {
	int cost;
	switch (n.kind()) {
		case ADD: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_ADD_38);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_ADD_37);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_ADD_36);
			}
			if (n.child1().is(NT_LAB) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_LAB) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_ADD_35);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_LAB)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_LAB) + 1;
				label(n, NT_REG, cost, R_REG_ADD_34);
			}
			break;
		}
		case AND: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_AND_15);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_AND_14);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_AND_13);
			}
			break;
		}
		case CBR: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_STMT) + 2;
			label(n, NT_BRANCH, cost, R_BRANCH_CBR_0);
			break;
		}
		case CMP_EQ: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_CMP_EQ_4);
			break;
		}
		case CMP_GE: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_CMP_GE_2);
			break;
		}
		case CMP_GT: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_CMP_GT_1);
			break;
		}
		case CMP_LE: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_CMP_LE_6);
			break;
		}
		case CMP_LT: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_CMP_LT_5);
			break;
		}
		case CMP_NE: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
			label(n, NT_REG, cost, R_REG_CMP_NE_3);
			break;
		}
		case CSTORE: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 2;
			label(n, NT_ASSIGN, cost, R_ASSIGN_CSTORE_3);
			break;
		}
		case CSTOREA: {
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_REG) && n.child1().child2().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 4;
				label(n, NT_ASSIGN, cost, R_ASSIGN_CSTOREA_2);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_REG) && n.child1().child2().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 3;
				label(n, NT_ASSIGN, cost, R_ASSIGN_CSTOREA_1);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_CON) && n.child1().child2().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 3;
				label(n, NT_ASSIGN, cost, R_ASSIGN_CSTOREA_0);
			}
			break;
		}
		case DIV: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_DIV_27);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_DIV_26);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_DIV_25);
			}
			break;
		}
		case ENTER: {
			cost = n.child1().cost(NT_STMT) + n.child2().cost(NT_STMT) + 0;
			label(n, NT_STMT, cost, R_STMT_ENTER_9);
			break;
		}
		case FUNAPP: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_JUMP) + 1;
			label(n, NT_REG, cost, R_REG_FUNAPP_52);
			break;
		}
		case LSHIFT: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_LSHIFT_21);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_LSHIFT_20);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_LSHIFT_19);
			}
			break;
		}
		case MOD: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_MOD_24);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_MOD_23);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_MOD_22);
			}
			break;
		}
		case MULT: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_MULT_30);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_MULT_29);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_MULT_28);
			}
			break;
		}
		case OR: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_OR_12);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_OR_11);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_OR_10);
			}
			break;
		}
		case RSHIFT: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_RSHIFT_18);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_RSHIFT_17);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_RSHIFT_16);
			}
			break;
		}
		case SIDEFFECTP: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_JUMP) + 1;
			label(n, NT_STMT, cost, R_STMT_SIDEFFECTP_6);
			break;
		}
		case STORE: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 2;
			label(n, NT_ASSIGN, cost, R_ASSIGN_STORE_7);
			break;
		}
		case STOREA: {
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_REG) && n.child1().child2().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 4;
				label(n, NT_ASSIGN, cost, R_ASSIGN_STOREA_6);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_REG) && n.child1().child2().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 3;
				label(n, NT_ASSIGN, cost, R_ASSIGN_STOREA_5);
			}
			if (n.child1().kind() == ADD && n.child1().child1().is(NT_CON) && n.child1().child2().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 3;
				label(n, NT_ASSIGN, cost, R_ASSIGN_STOREA_4);
			}
			break;
		}
		case SUB: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_SUB_33);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_SUB_32);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_SUB_31);
			}
			break;
		}
		case TBL: {
			cost = n.child1().cost(NT_REG) + n.child2().cost(NT_LAB) + 0;
			label(n, NT_STMT, cost, R_STMT_TBL_0);
			break;
		}
		case XOR: {
			if (n.child1().is(NT_REG) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_XOR_9);
			}
			if (n.child1().is(NT_REG) && n.child2().is(NT_CON)) {
				cost = n.child1().cost(NT_REG) + n.child2().cost(NT_CON) + 1;
				label(n, NT_REG, cost, R_REG_XOR_8);
			}
			if (n.child1().is(NT_CON) && n.child2().is(NT_REG)) {
				cost = n.child1().cost(NT_CON) + n.child2().cost(NT_REG) + 1;
				label(n, NT_REG, cost, R_REG_XOR_7);
			}
			break;
		}
		default: {
			throw new AssertionError("ERROR - label_2(): Unhandeled Node kind: " + n.kind());
		}
	} // END SWITCH
} // END METHOD label_2()

/**
 * closure():
 *   Calculate the transitive closure for this Node.
 */
private static void closure (Node n, NT nt, RuleEnum r) {
	switch (nt) {
		case NT_STMT: {
			label (n, NT_ROOT, n.cost(nt), R_ROOT_STMT_0); break;
		}
		case NT_REG: {
			label (n, NT_STMT, n.cost(nt), R_STMT_REG_2); break;
		}
		case NT_JUMP: {
			label (n, NT_STMT, n.cost(nt), R_STMT_JUMP_3); break;
		}
		case NT_BRANCH: {
			label (n, NT_STMT, n.cost(nt), R_STMT_BRANCH_4); break;
		}
		case NT_ASSIGN: {
			label (n, NT_STMT, n.cost(nt), R_STMT_ASSIGN_5); break;
		}
		case NT_CON: {
			label (n, NT_REG, n.cost(nt), R_REG_CON_54); break;
		}
		case NT_LAB: {
			label (n, NT_REG, n.cost(nt), R_REG_LAB_56); break;
		}
	}
} // END METHOD closure()


} // END CLASS Tiling




// Eval Class
private static class Eval {





private static void eval_root (Node n) {
	RuleEnum r = n.rule(NT_ROOT);
	
	switch (r) {
	
		case R_ROOT_STMT_0: {
			// (:
	System.out.println("Starting Emitting Code....");	
			// :)
			eval_stmt(n);
			Node st1 = n;
			// (:
	System.out.println("End of Code emission.");	
			// :)
	
			break;
	
		}
	
		default: {
	
			throw new AssertionError("ERROR: Unhandeled semantic rule - " + r +".");
	
		}
	
	}
} // END METHOD eval_root()


private static void eval_stmt (Node n) {
	RuleEnum r = n.rule(NT_STMT);
	
	switch (r) {
	
		case R_STMT_TBL_0: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_lab(n.child2());
			Node l1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_STMT_NOP_1: {
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_STMT_REG_2: {
			String r1 = eval_reg(n);
	
			break;
	
		}
	
		case R_STMT_JUMP_3: {
			eval_jump(n);
	
			break;
	
		}
	
		case R_STMT_BRANCH_4: {
			eval_branch(n);
	
			break;
	
		}
	
		case R_STMT_ASSIGN_5: {
			eval_assign(n);
	
			break;
	
		}
	
		case R_STMT_SIDEFFECTP_6: {
			String r1 = eval_reg(n.child1());
			eval_jump(n.child2());
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_STMT_SIDEFFECT_7: {
			eval_jump(n.child1());
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_STMT_FUN_8: {
			eval_stmt(n.child1());
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_STMT_ENTER_9: {
			eval_stmt(n.child1());
			eval_stmt(n.child2());
	
			break;
	
		}
	
		default: {
	
			throw new AssertionError("ERROR: Unhandeled semantic rule - " + r +".");
	
		}
	
	}
} // END METHOD eval_stmt()


private static void eval_assign (Node n) {
	RuleEnum r = n.rule(NT_ASSIGN);
	
	switch (r) {
	
		case R_ASSIGN_CSTOREA_0: {
			String r = eval_con(n.child1().child1());
			Node n1 = n.child1().child1();
			String r = eval_reg(n.child1().child2());
			Node r1 = n.child1().child2();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_ASSIGN_CSTOREA_1: {
			String r = eval_reg(n.child1().child1());
			Node r1 = n.child1().child1();
			String r = eval_con(n.child1().child2());
			Node n1 = n.child1().child2();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_ASSIGN_CSTOREA_2: {
			Node cs1 = n;
			Node a1 = n.child1();
			String r = eval_reg(n.child1().child1());
			Node r1 = n.child1().child1();
			String r = eval_reg(n.child1().child2());
			Node r2 = n.child1().child2();
			String r = eval_reg(n.child2());
			Node r3 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_ASSIGN_CSTORE_3: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_ASSIGN_STOREA_4: {
			String r = eval_con(n.child1().child1());
			Node n1 = n.child1().child1();
			String r = eval_reg(n.child1().child2());
			Node r1 = n.child1().child2();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_ASSIGN_STOREA_5: {
			String r = eval_reg(n.child1().child1());
			Node r1 = n.child1().child1();
			String r = eval_con(n.child1().child2());
			Node n1 = n.child1().child2();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_ASSIGN_STOREA_6: {
			String r = eval_reg(n.child1().child1());
			Node r1 = n.child1().child1();
			String r = eval_reg(n.child1().child2());
			Node r2 = n.child1().child2();
			String r = eval_reg(n.child2());
			Node r3 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_ASSIGN_STORE_7: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		default: {
	
			throw new AssertionError("ERROR: Unhandeled semantic rule - " + r +".");
	
		}
	
	}
} // END METHOD eval_assign()


private static void eval_branch (Node n) {
	RuleEnum r = n.rule(NT_BRANCH);
	
	switch (r) {
	
		case R_BRANCH_CBR_0: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			eval_stmt(n.child2());
			Node s = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		default: {
	
			throw new AssertionError("ERROR: Unhandeled semantic rule - " + r +".");
	
		}
	
	}
} // END METHOD eval_branch()


private static void eval_jump (Node n) {
	RuleEnum r = n.rule(NT_JUMP);
	
	switch (r) {
	
		case R_JUMP_JUMPI_0: {
			String r = eval_lab(n.child1());
			Node l1 = n.child1();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_JUMP_JUMP_1: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		default: {
	
			throw new AssertionError("ERROR: Unhandeled semantic rule - " + r +".");
	
		}
	
	}
} // END METHOD eval_jump()


private static String eval_lab (Node n) {
	RuleEnum r = n.rule(NT_LAB);
	
	String result;
	// (:
 String result = null; 
	// :)
	
	switch (r) {
	
		case R_LAB_LAB_0: {
			Node l1 = n;
			// (:

					result = l1.name;
				
			// :)
	
			break;
	
		}
	
		default: {
	
			throw new AssertionError("ERROR: Unhandeled semantic rule - " + r +".");
	
		}
	
	}return result;

} // END METHOD eval_lab()


private static String eval_con (Node n) {
	RuleEnum r = n.rule(NT_CON);
	
	String result;
	// (:
 String result = null; 
	// :)
	
	switch (r) {
	
		case R_CON_NUM_0: {
			Node n1 = n;
			// (:

					result = n1.name;
				
			// :)
	
			break;
	
		}
	
		default: {
	
			throw new AssertionError("ERROR: Unhandeled semantic rule - " + r +".");
	
		}
	
	}return result;

} // END METHOD eval_con()


private static String eval_reg (Node n) {
	RuleEnum r = n.rule(NT_REG);
	
	String result;
	// (:
 String result = null; 
	// :)
	
	switch (r) {
	
		case R_REG_RETURN_0: {
			String r = eval_reg(n.child1());
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CMP_GT_1: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CMP_GE_2: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CMP_NE_3: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CMP_EQ_4: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CMP_LT_5: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CMP_LE_6: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_XOR_7: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
	
			break;
	
		}
	
		case R_REG_XOR_8: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
	
			break;
	
		}
	
		case R_REG_XOR_9: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
	
			break;
	
		}
	
		case R_REG_OR_10: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
	
			break;
	
		}
	
		case R_REG_OR_11: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
	
			break;
	
		}
	
		case R_REG_OR_12: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
	
			break;
	
		}
	
		case R_REG_AND_13: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
	
			break;
	
		}
	
		case R_REG_AND_14: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
	
			break;
	
		}
	
		case R_REG_AND_15: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
	
			break;
	
		}
	
		case R_REG_RSHIFT_16: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_RSHIFT_17: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_RSHIFT_18: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_LSHIFT_19: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_LSHIFT_20: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_LSHIFT_21: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_MOD_22: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_MOD_23: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_MOD_24: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_DIV_25: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_DIV_26: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_DIV_27: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_MULT_28: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_MULT_29: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_MULT_30: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_SUB_31: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_SUB_32: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_SUB_33: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_ADD_34: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_lab(n.child2());
			Node l1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_ADD_35: {
			String r = eval_lab(n.child1());
			Node l1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_ADD_36: {
			String r = eval_con(n.child1());
			Node n1 = n.child1();
			String r = eval_reg(n.child2());
			Node r1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_ADD_37: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_con(n.child2());
			Node n1 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_ADD_38: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			String r = eval_reg(n.child2());
			Node r2 = n.child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CLOADA_39: {
			String r = eval_con(n.child1().child1());
			Node n1 = n.child1().child1();
			String r = eval_reg(n.child1().child2());
			Node r2 = n.child1().child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CLOADA_40: {
			String r = eval_reg(n.child1().child1());
			Node r1 = n.child1().child1();
			String r = eval_con(n.child1().child2());
			Node n1 = n.child1().child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CLOADA_41: {
			String r = eval_reg(n.child1().child1());
			Node r1 = n.child1().child1();
			String r = eval_reg(n.child1().child2());
			Node r2 = n.child1().child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CLOAD_42: {
			String r = eval_reg(n.child1());
			Node r1 = n.child1();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_LOADA_43: {
			String r = eval_lab(n.child1().child1());
			Node l1 = n.child1().child1();
			String r = eval_reg(n.child1().child2());
			Node r1 = n.child1().child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_LOADA_44: {
			String r = eval_reg(n.child1().child1());
			Node r2 = n.child1().child1();
			String r = eval_lab(n.child1().child2());
			Node l1 = n.child1().child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_LOADA_45: {
			String r = eval_con(n.child1().child1());
			Node n1 = n.child1().child1();
			String r = eval_reg(n.child1().child2());
			Node r2 = n.child1().child2();
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_LOADA_46: {
			String r = eval_reg(n.child1().child1());
			Node r1 = n.child1().child1();
			String r = eval_con(n.child1().child2());
			Node n1 = n.child1().child2();
			// (:

					result = nreg();
					l.add(label(n) + "  lw " + result + "," + ar1);
				
			// :)
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_LOAD_47: {
			String r1 = eval_reg(n.child1());
			// (:

					result = nreg();
					l.add(label(n) + "  lw " + result + "," + r1);
				
			// :)
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_I2C_48: {
			String r1 = eval_reg(n.child1());
			// (:

					l.add("# @XXX: UNIMPLEMENTED I2C(reg <:out String r:> r1)");
				
			// :)
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_C2I_49: {
			String r1 = eval_reg(n.child1());
			// (:

					l.add("# @XXX: UNIMPLEMENTED C2I(reg <:out String r:> r1)");
				
			// :)
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_C2C_50: {
			String r1 = eval_reg(n.child1());
			// (:

					l.add("# @XXX: UNIMPLEMENTED C2C(reg <:out String r:> r1)");
				
			// :)
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_I2I_51: {
			String r1 = eval_reg(n.child1());
			// (:

					l.add("# @XXX: UNIMPLEMENTED I2I(reg <:out String r:> r1)");
				
			// :)
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_FUNAPP_52: {
			Node f1 = n;
			// (:

						if (f1.label != null)
							l.add(label(f1));
					
			// :)
			String r1 = eval_reg(n.child1());
			eval_jump(n.child2());
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_FUNAP_53: {
			Node f1 = n;
			// (:

						if (f1.label != null)
							l.add(label(f1));
					
			// :)
			eval_jump(n.child1());
			if (n.link() != null) {
				eval_stmt(n.link());
			}
	
			break;
	
		}
	
		case R_REG_CON_54: {
			String r = eval_con(n);
			Node n1 = n;
			// (:

					result = nreg();
					String instr = label(n1) + "  li " + result + "," + n1.name;
					l.add(instr);
				
			// :)
	
			break;
	
		}
	
		case R_REG_VAL_55: {
			Node v1 = n;
			// (:
 /* value known to be in register like r_arp -:> Activation Record PTR */
					result = v1.name;
				
			// :)
	
			break;
	
		}
	
		case R_REG_LAB_56: {
			String r = eval_lab(n);
			Node l1 = n;
			// (:

					result = nreg();
					String instr = label(l1) + "  li " + result + "," + r;
					l.add(instr);
				
			// :)
	
			break;
	
		}
	
		default: {
	
			throw new AssertionError("ERROR: Unhandeled semantic rule - " + r +".");
	
		}
	
	}return result;

} // END METHOD eval_reg()


} // END CLASS Eval



} // END CLASS Codegen

