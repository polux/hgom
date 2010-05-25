package modpol;
import tom.library.sl.*;

abstract public class Policy extends AbstractStrategyBasic {

  %include{ sl.tom }
  %include{ query.tom }

  public Policy(){
     super(`Identity());
  }

  // to be overridden
//  %strategy Default() extends Identity(){
//    visit Query {
//      q@_ -> { return null; }
//    }
//  }  

  // to be overridden
  public abstract Query compute(Query q);
  public abstract tom.library.sl.Introspector getPolicyIntrospector();

  public <T> T visitLight(T req,Introspector introspector) throws VisitFailure{
    if(req instanceof Query){
      Query q = (Query) req;
      return (T)compute(q); // this is bullshit
    }
    else return any.visitLight(req,introspector); //throw new VisitFailure("Policy::visitLight(T,Introspector): first argument not of type Query");
  }



}
