package modpol;
import tom.library.sl.*; // imports the runtime strategy library

// pour l'exemple
import poltemps.*;

public class SequencePolicy extends Policy{

  %include { query.tom } 
  // includes description of strategy operators for tom 
  %include { sl.tom } 

  private Policy pol1;
  private Policy pol2;

  public SequencePolicy(Policy p1,Policy p2){ pol1 = p1; pol2 = p2; }

//  %strategy First() extends Identity(){
//    visit Query{
//      q@query() -> { pol1.query(`q); }
//    }
//  }


  public Query compute(Query q){
    //TODO: should be set in the classes of pol1 and pol2
    pol1.setIntrospector(pol1.getPolicyIntrospector());
    pol2.setIntrospector(pol2.getPolicyIntrospector());

    try {
      return `Sequence(pol1,pol2).visit(q, null);
      /**    
        Query q1 = pol1.visit(q,pol1.getPolicyIntrospector());
        if (q1 instanceof Permit) {
        return q1;
        } else {
        return pol2.visit(q,pol2.getPolicyIntrospector());
        }
       */
    } catch (tom.library.sl.VisitFailure e) {
      throw new RuntimeException("unexpected visitfailure "+e);
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
