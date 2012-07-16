// watch 'gcc -lobjc -framework Foundation ivar-shadowing.m 2>&1 && ./a.out  2>&1'
#import <Foundation/Foundation.h>
#import <objc/objc.h>
#import <objc/runtime.h>
#import "trace.h"

// playing around with getting/setting instance variables in multiple
// layers of a class hierarchy.

int main() {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    Class A;
    trace(p,   A = objc_allocateClassPair([NSObject class], "A", 0));
    trace(hhd, class_addIvar(A, "foo", sizeof(int), sizeof(int), @encode(int)));
    trace(hhd, class_addIvar(A, "bar", sizeof(float), sizeof(float), @encode(float)));
    trace(objc_registerClassPair(A));
    trace();
    
    Class B;
    trace(p,   B = objc_allocateClassPair(A, "B", 0));
    trace(hhd, class_addIvar(B, "foo", sizeof(int), sizeof(int), @encode(int)));
    trace(objc_registerClassPair(B));
    trace();
    
    // There are two different "foo" ivars.
    Ivar A_foo;
    Ivar B_foo;
    trace(p, A_foo = class_getInstanceVariable(A, "foo"));
    trace(p, B_foo = class_getInstanceVariable(B, "foo"));
    trace(d, A_foo == B_foo);
    trace();
    
    // there is only one "bar" ivar, and class_getInstanceVariable
    // will find it from the subclass as well as the superclass.
    Ivar A_bar;
    Ivar B_bar;
    trace(p, A_bar = class_getInstanceVariable(A, "bar"));
    trace(p, B_bar = class_getInstanceVariable(B, "bar"));
    trace(d, A_bar == B_bar);
    trace();
    
    id b;
    trace(p, b = [[B alloc] init]);
    trace();
    
    // so 'b' has 2 instance variables named "foo", and getInstanceVariable
    // and setInstanceVariable use the one associated with the subclass.
    Ivar b_foo;
    int x;
    trace(p, b_foo = object_setInstanceVariable(b, "foo", (x = 42, &x)));
    trace(d, A_foo == b_foo);
    trace(d, B_foo == b_foo);
    trace();
    
    [pool release];
    return 0;
}