def letRec[X](x: (=>X)=>X): X =
{
    lazy val xx:X = x(xx)
    xx
}

def letRec[X,Y](x: (=>X,=>Y)=>X,
                y: (=>X,=>Y)=>Y): (X,Y) =
{
    object rec {
        lazy val xx:X = x(xx,yy)
        lazy val yy:Y = y(xx,yy)
    }
    (rec.xx,rec.yy)
}

def letRec[X,Y,Z](x: (=>X,=>Y,=>Z)=>X,
                  y: (=>X,=>Y,=>Z)=>Y,
                  z: (=>X,=>Y,=>Z)=>Z): (X,Y,Z) =
{
    object rec {
        lazy val xx:X = x(xx,yy,zz)
        lazy val yy:Y = y(xx,yy,zz)
        lazy val zz:Z = z(xx,yy,zz)
    }
    (rec.xx,rec.yy,rec.zz)
}

def letRec[W,X,Y,Z](w: (=>W,=>X,=>Y,=>Z)=>W,
                    x: (=>W,=>X,=>Y,=>Z)=>X,
                    y: (=>W,=>X,=>Y,=>Z)=>Y,
                    z: (=>W,=>X,=>Y,=>Z)=>Z): (W,X,Y,Z) =
{
    object rec {
        lazy val ww:W = w(ww,xx,yy,zz)
        lazy val xx:X = x(ww,xx,yy,zz)
        lazy val yy:Y = y(ww,xx,yy,zz)
        lazy val zz:Z = z(ww,xx,yy,zz)
    }
    (rec.ww,rec.xx,rec.yy,rec.zz)
}

