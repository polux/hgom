package modpol;
import tom.library.sl.*; // imports the runtime strategy library

// pour l'exemple
import poltemps.*;

public class SequencePolicy extends Policy{


private static boolean tom_equal_term_Query(Object c1, Object c2) {
return  c1.equals(c2) ;
}
private static boolean tom_is_sort_Query(Object c) {
return  c instanceof Query ;
}

// includes description of strategy operators for tom 

private static boolean tom_equal_term_Strategy(Object t1, Object t2) {
return  (t1.equals(t2)) ;
}
private static boolean tom_is_sort_Strategy(Object t) {
return  (t instanceof tom.library.sl.Strategy) ;
}
private static boolean tom_equal_term_Position(Object t1, Object t2) {
return  (t1.equals(t2)) ;
}
private static boolean tom_is_sort_Position(Object t) {
return  (t instanceof tom.library.sl.Position) ;
}
private static boolean tom_equal_term_int(int t1, int t2) {
return  t1==t2 ;
}
private static boolean tom_is_sort_int(int t) {
return  true ;
}
private static boolean tom_equal_term_char(char t1, char t2) {
return  t1==t2 ;
}
private static boolean tom_is_sort_char(char t) {
return  true ;
}
private static boolean tom_equal_term_String(String t1, String t2) {
return  t1.equals(t2) ;
}
private static boolean tom_is_sort_String(String t) {
return  t instanceof String ;
}


private Policy pol1;
private Policy pol2;

public SequencePolicy(Policy p1,Policy p2){ pol1 = p1; pol2 = p2; }

//  %strategy First() extends Identity(){
//    visit Query{
//      q@query() -> { pol1.query(`q); }
//    }
//  }


public Query compute(Query q){
//    return `Sequence(pol1,pol2).visit(q);
try {
Query q1 = pol1.visit(q,pol1.getPolicyIntrospector());
if (q1 instanceof Permit) {
return q1;
} else {
return pol2.visit(q,pol2.getPolicyIntrospector());
}
} catch (tom.library.sl.VisitFailure e) {
return q;
}
}

public tom.library.sl.Introspector getPolicyIntrospector() {
return null;
}
//    // Query answer1 = pol1.query(q);
//    PolTemps p1 = (PolTemps) pol1;
//    Strategy s1 = p1.getStrat();
//    PolTemps p2 = (PolTemps) pol2;
//    Strategy s2 = p2.getStrat();
//    Query answer2 = pol2.query(q);
//    Strategy s2 = `Identity.visitLight(answer2,new LocalIntrospector());
//    return `Sequence(s1,s2).visit(q,new LocalIntrospector());
//    return null;
//  }
//    Query answer1 = pol1.query(q);
//    Query answer2 = pol2.query(q);
//
//    // /* debug */ System.out.println("tmpstate: "+answer1+" / "+answer2);
//
//    %match(Query answer1, Query answer2){
//      permit,_ -> { return `permit(); }
//      _,permit -> { return `permit(); }
//      deny,deny -> { return `deny(); }
//      deny,notApplicable -> { return `deny(); }
//      notApplicable,deny -> { return `deny(); }
//      notApplicable,notApplicable -> { return `deny(); } 
//    }
//    System.err.println("Error in PermitOverrides::query(Query)");
//    return null;
//  }
}
