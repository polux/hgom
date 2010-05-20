package polrbac;
import modpol.*;
import tom.library.sl.*;

class PolicyRBAC extends Policy{

  %include { ../modpol/permit.tom }
  %include { ../modpol/notApplicable.tom }
  %include { queryCreateSession.tom }
  %include { queryDeleteSession.tom }
  %include { sl.tom }

//  %include { sl.tom }
//
//  %strategy Default() extends Identity(){
//    visit Query{
//	   queryCreateSession() -> { return `permit(); }
//	   queryDeleteSession() -> { return `permit(); }
//	   _ -> { return `notApplicable(); }
//    }
//    System.err.println("Error in PolicyRBAC::query(), matching failed!");
//    return null;
//  } 

  %strategy MyStrat() extends Identity() {
    visit Query {
      x -> { System.out.println(`x); }
    }
  }

  public tom.library.sl.Introspector getPolicyIntrospector() {
    return new LocalIntrospector();
  }

  public Query compute(Query q){
    %match(q){
      queryCreateSession() -> { return `permit(); }
      queryDeleteSession() -> { return `permit(); }
      _ -> { return `notApplicable(); }
    }
    return null; // because the compiler sucks
  } 


}
