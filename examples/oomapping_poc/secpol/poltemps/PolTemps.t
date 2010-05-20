package poltemps;
import modpol.*;

// pour le main uniquement
import polrbac.*;
import tom.library.sl.*;

class PolTemps extends Policy{

  %include { queryTemps.tom }
  %include { ../modpol/deny.tom }
  %include { ../modpol/notApplicable.tom }
  %include { ../sl.tom }

//  %strategy Default() extends Identity(){
//    visit Query{
//      queryTemps(h,_,s) && (9 <= h && h <= 18) -> { return `s; } // pas besoin de savoir que c'est par dessus RBAC!
//      queryTemps(h,t,s) && (t == 1 && (h < 9 || 18 < h)) -> { return `s; }
//      queryTemps(h,t,_) && (t == 0 && (h < 9 || 18 < h)) -> { return `deny(); }
//      _ -> { return `notApplicable(); }
//    }
////    System.out.println("Error in PolTemps::query(Query)");
////    return null;
//  } 

  public tom.library.sl.Introspector getPolicyIntrospector() {
    return new LocalIntrospector();
  }
  public Query compute(Query q){
    %match(q){
      queryTemps(h,_,s) && (9 <= h && h <= 18) -> { return `s; } // pas besoin de savoir que c'est par dessus RBAC!
      queryTemps(h,t,s) && (t == 1 && (h < 9 || 18 < h)) -> { return `s; }
      queryTemps(h,t,_) && (t == 0 && (h < 9 || 18 < h)) -> { return `deny(); }
      _ -> { return `notApplicable(); }
    }
    //    System.out.println("Error in PolTemps::query(Query)");
    return null; // because the compiler sucks
  } 

//  %include { ../modpol/queryError.tom }
//
//  %strategy PolTempsStrat() extends Identity(){
//     visit Query {
//	q@queryTemps(_,_,_) -> { return compute(`q); }
//	_ -> { return `queryError(); }
//    }
//  }

  public static void main(String[] args){
    QueryCreateSession qcs = new QueryCreateSession();
    QueryDeleteSession qds = new QueryDeleteSession();
    QueryTemps qt0 = new QueryTemps(qcs,0); // Denied
    QueryTemps qt1 = new QueryTemps(qcs,9); // Permitted
    QueryTemps qt2 = new QueryTemps(qds,0); // Permitted
    QueryTemps qt3 = new QueryTemps(qds,9); // Permitted

    PolTemps pt = new PolTemps();

    System.out.println(pt.compute(qt0));
    System.out.println(pt.compute(qt1));
    System.out.println(pt.compute(qt2));
    System.out.println(pt.compute(qt3));

    System.out.println("");

    Policy po = new SequencePolicy(pt,pt);
    System.out.println(po.compute(qt0) + " should be denied"); // Denied
    System.out.println(po.compute(qt1) + " should be permitted"); // Permitted
  }
  %strategy MyStrat() extends Identity() {
    visit Query {
      x -> { System.out.println(`x); }
    }
  }


}
