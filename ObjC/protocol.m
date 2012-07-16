#import <Foundation/Foundation.h>
#import <objc/objc.h>
#import <objc/runtime.h>
#import "trace.h"

@protocol P
- (int) favoriteNum;
@end

@interface A : NSObject <P> {}
@end
@implementation A
- (int) favoriteNum { return 42; }
@end

@interface B : A {}
@end
@implementation B
@end

@interface C : NSObject {}
- (int) favoriteNum;
@end
@implementation C
- (int) favoriteNum { return 6 * 9;}
@end

int main() {
    trace(hhd, [A conformsToProtocol: @protocol(P)]);
    trace(hhd, [B conformsToProtocol: @protocol(P)]);
    trace(hhd, [C conformsToProtocol: @protocol(P)]);
    return 0;
}