package oomapping;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.framework.Assert;

import java.util.List;

import base.hand.types.*;
import base.hand.types.t1.*;
import base.hand.types.t2.*;
import tom.library.sl.*;
import oomapping.signature.ISignature;

public class OOMappingTest extends TestCase {
  
  %include { sl.tom }
  %include { signature/Signature.tom }
  
  /*
   * the body of the following method is hand-written
   */
  public static ISignature getSignature() {
    return signature;
  }

  /* code by hand */
  private static ISignature signature = new ISignature() {
    private Module.a_Mapping a_Mapping = new Module.a_Mapping();
    private Module.b_Mapping b_Mapping = new Module.b_Mapping();
    private Module.f_Mapping f_Mapping = new Module.f_Mapping();
    private Module.g_Mapping g_Mapping = new Module.g_Mapping();
    //private Module.h_Mapping h_Mapping = new Module.h_Mapping();
    //private Module.concT1_Mapping concT1_Mapping = new Module.concT1_Mapping();

    public tom.library.oomapping.Mapping0<T1>       getMapping_a() { return a_Mapping; }
    public tom.library.oomapping.Mapping0<T2>       getMapping_b() { return b_Mapping; }
    public tom.library.oomapping.Mapping2<T1,T1,T2> getMapping_f() { return f_Mapping; }
    public tom.library.oomapping.Mapping1<T2,T2>    getMapping_g() { return g_Mapping; }
    //public tom.library.oomapping.Mapping1<T2,java.util.List<T1>>    getMapping_h() { return h_Mapping; }
    //public tom.library.oomapping.ListMapping<java.util.List<T1>,T1>    getMapping_concT1() { return concT1_Mapping; }
  };


  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(OOMappingTest.class));
  }

  public void testMatch() {
    T1 subject = `f(f(a(),b()),g(b()));
    %match(subject) {
      f(x,g(y)) -> { 
        assertEquals(`x,`f(a(),b()));
        assertEquals(`y,`b());
        return;
      }
    }
    fail();
  }

  public void testVisit() {
    T1 subject = `f(f(a(),b()),g(b()));
    try {
      T1 res1 = (T1)
`Repeat(OnceBottomUp(Rule())).visitLight(subject,tom.library.oomapping.Introspector.getInstance());
      T1 res2 = (T1)
`Repeat(OnceBottomUp(Rule())).visit(subject,tom.library.oomapping.Introspector.getInstance());
      assertEquals(res1, `a());
      assertEquals(res1, res2);
    } catch(VisitFailure e) {
      fail();
    }

  }

/**
  public void testCongruence() {
    T1 subject = `f(f(a(),b()),g(b()));
    try {
      T1 res1 = (T1) `_f(Rule(), Rule2()).visitLight(subject,tom.library.oomapping.Introspector.getInstance());
      T1 res2 = (T1) `_f(Rule(), Rule2()).visit(subject,tom.library.oomapping.Introspector.getInstance());
      assertEquals(res1, `f(a(),b()));
      assertEquals(res1, res2);
    } catch(VisitFailure e) {
      fail();
    }
  }

  public void test_listMatchCongruence() {
    List<T1> subject = `concT1(f(a(),b()),a(),f(a(),b()));
    try {
      List<T1> res1 = (List<T1>) `_concT1(Try(Rule())).visitLight(subject,tom.library.oomapping.Introspector.getInstance());
      List<T1> res2 = (List<T1>) `_concT1(Try(Rule())).visit(subject,tom.library.oomapping.Introspector.getInstance());
      //List<T1> res2 = (List<T1>) `_concT1(Try(Sequence(Print(),Rule()))).visit(subject,tom.library.oomapping.Introspector.getInstance());
      assertEquals(res1, `concT1(a(),a(),a()));
      //assertEquals(res1, res2);
    } catch(VisitFailure e) {
      fail();
    }
  }
*/

  %strategy Print() extends Identity() {
    visit T1 {
      x -> { System.out.println(`x /*+ " : " + getPosition()*/); }
    }
    visit T2 {
      x -> { System.out.println(`x /*+ " : " + getPosition()*/); }
    }
  }

  %strategy Rule() extends Fail() {
    visit T1 {
      f(x,y) -> {
        return `x;
      }
    }
  }

  %strategy Rule2() extends Fail() {
    visit T2 {
      g(x) -> x
    }
  }

/**
  public void test_listMatchFirst() {
    List<T1> subject = `concT1(a(), f(a(), b()), f(a(), g(b())));

    %match(subject) {
      concT1(first, _*) -> {

        assertEquals(`first, `a());
        return;
      }
    }
    fail();
  }

  public void test_listMatchLast() {
    Object subject = `concT1(a(), f(a(), b()), f(a(), g(b())));

    %match(subject) {
      concT1(_*, last) -> {

        assertEquals(`last, `f(a(), g(b())));
        return;
      }
    }
    fail();
  }

  public void test_listMatchCount() {
    T2 subject = `h(concT1(a(), f(a(), b()), f(a(), g(b()))));

    int i = 0;
    %match(subject) {
      h(concT1(_*, x, _*)) -> {
        i++;
      }
    }
    assertEquals(i,3);
  }

  public void test_listMatchNonLinear() {
    T2 subject = `h(concT1(a(), f(a(), b()), f(a(),b()), a(), f(a(), g(b()))));

    %match(subject) {
      h(concT1(X1*, x, X2*, x, X3*)) -> {
        assertEquals(`x,`a());
        assertEquals(`X1,`concT1());
        assertEquals(`X2,`concT1(f(a(),b()), f(a(),b())));
        assertEquals(`X3,`concT1(f(a(), g(b()))));
        return;
      }
      _ -> {
        fail();
      }
    }
  }

  public void test_listMatchAny() {
    T2 subject = `h(concT1(a(), f(a(), b()), f(a(), g(b()))));

    %match(subject) {
      !h(concT1(_*, a(), _*)) -> {
        fail();
        return;
      }
    }
  }

  public void test_congWithListMatch() {
    T2 subject = `h(concT1(a(), f(a(), b()), f(a(), g(b()))));

    // todo: how to do this?
    //`_h(_concT1(map(Print()))).visitLight(subject);
  }
*/
}
