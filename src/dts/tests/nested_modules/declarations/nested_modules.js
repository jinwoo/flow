declare module M {
  declare var N: $Exports<'M___N'>;
  declare var x: number;
  declare class C {
    y: typeof x;

  }
  declare var m: typeof N.z;
}
declare module M___N {
  declare var M: $Exports<'M'>;
  declare var x : typeof M.x;
  declare var C : typeof M.C;
  declare var m : typeof M.m;
  declare var z: typeof x;
}
declare module P {
  declare var M: $Exports<'M'>;
  declare var a: typeof M.N.z;
}
