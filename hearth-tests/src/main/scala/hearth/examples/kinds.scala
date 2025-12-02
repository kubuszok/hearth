package hearth
package examples
package kinds

trait Arity1[A]
trait Arity2[A, B]
trait Arity3[A, B, C]
trait Arity4[A, B, C, D]
trait Arity5[A, B, C, D, E]
trait Arity6[A, B, C, D, E, F]
trait Arity7[A, B, C, D, E, F, G]
trait Arity8[A, B, C, D, E, F, G, H]
trait Arity9[A, B, C, D, E, F, G, H, I]
trait Arity10[A, B, C, D, E, F, G, H, I, J]
trait Arity11[A, B, C, D, E, F, G, H, I, J, K]
trait Arity12[A, B, C, D, E, F, G, H, I, J, K, L]
trait Arity13[A, B, C, D, E, F, G, H, I, J, K, L, M]
trait Arity14[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
trait Arity15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
trait Arity16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
trait Arity17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
trait Arity18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
trait Arity19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
trait Arity20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
trait Arity21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
trait Arity22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]

// Only needed for type aliasbwith 22 parameters (fixing 1 parameter).
// We do not support Type.Ctor23.of[...] and above.
trait Arity23[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]

// format: off
object Alias {
  type Renamed1[A] = Arity1[A]
  type Extra1[A] = Unit
  type FixedFront1[A] = Arity2[Unit, A]
  type FixedBack1[A] = Arity2[A, Unit]

  type Renamed2[A, B] = Arity2[A, B]
  type Extra2[A, B] = Arity1[A]
  type FixedFront2[A, B] = Arity3[Unit, A, B]
  type FixedBack2[A, B] = Arity3[A, B, Unit]

  type Renamed3[A, B, C] = Arity3[A, B, C]
  type Extra3[A, B, C] = Arity2[A, B]
  type FixedFront3[A, B, C] = Arity4[Unit, A, B, C]
  type FixedBack3[A, B, C] = Arity4[A, B, C, Unit]

  type Renamed4[A, B, C, D] = Arity4[A, B, C, D]
  type Extra4[A, B, C, D] = Arity3[A, B, C]
  type FixedFront4[A, B, C, D] = Arity5[Unit, A, B, C, D]
  type FixedBack4[A, B, C, D] = Arity5[A, B, C, D, Unit]

  type Renamed5[A, B, C, D, E] = Arity5[A, B, C, D, E]
  type Extra5[A, B, C, D, E] = Arity4[A, B, C, D]
  type FixedFront5[A, B, C, D, E] = Arity6[Unit, A, B, C, D, E]
  type FixedBack5[A, B, C, D, E] = Arity6[A, B, C, D, E, Unit]

  type Renamed6[A, B, C, D, E, F] = Arity6[A, B, C, D, E, F]
  type Extra6[A, B, C, D, E, F] = Arity5[A, B, C, D, E]
  type FixedFront6[A, B, C, D, E, F] = Arity7[Unit, A, B, C, D, E, F]
  type FixedBack6[A, B, C, D, E, F] = Arity7[A, B, C, D, E, F, Unit]

  type Renamed7[A, B, C, D, E, F, G] = Arity7[A, B, C, D, E, F, G]
  type Extra7[A, B, C, D, E, F, G] = Arity6[A, B, C, D, E, F]
  type FixedFront7[A, B, C, D, E, F, G] = Arity8[Unit, A, B, C, D, E, F, G]
  type FixedBack7[A, B, C, D, E, F, G] = Arity8[A, B, C, D, E, F, G, Unit]

  type Renamed8[A, B, C, D, E, F, G, H] = Arity8[A, B, C, D, E, F, G, H]
  type Extra8[A, B, C, D, E, F, G, H] = Arity7[A, B, C, D, E, F, G]
  type FixedFront8[A, B, C, D, E, F, G, H] = Arity9[Unit, A, B, C, D, E, F, G, H]
  type FixedBack8[A, B, C, D, E, F, G, H] = Arity9[A, B, C, D, E, F, G, H, Unit]

  type Renamed9[A, B, C, D, E, F, G, H, I] = Arity9[A, B, C, D, E, F, G, H, I]
  type Extra9[A, B, C, D, E, F, G, H, I] = Arity8[A, B, C, D, E, F, G, H]
  type FixedFront9[A, B, C, D, E, F, G, H, I] = Arity10[Unit, A, B, C, D, E, F, G, H, I]
  type FixedBack9[A, B, C, D, E, F, G, H, I] = Arity10[A, B, C, D, E, F, G, H, I, Unit]

  type Renamed10[A, B, C, D, E, F, G, H, I, J] = Arity10[A, B, C, D, E, F, G, H, I, J]
  type Extra10[A, B, C, D, E, F, G, H, I, J] = Arity9[A, B, C, D, E, F, G, H, I]
  type FixedFront10[A, B, C, D, E, F, G, H, I, J] = Arity11[Unit, A, B, C, D, E, F, G, H, I, J]
  type FixedBack10[A, B, C, D, E, F, G, H, I, J] = Arity11[A, B, C, D, E, F, G, H, I, J, Unit]

  type Renamed11[A, B, C, D, E, F, G, H, I, J, K] = Arity11[A, B, C, D, E, F, G, H, I, J, K]
  type Extra11[A, B, C, D, E, F, G, H, I, J, K] = Arity10[A, B, C, D, E, F, G, H, I, J]
  type FixedFront11[A, B, C, D, E, F, G, H, I, J, K] = Arity12[Unit, A, B, C, D, E, F, G, H, I, J, K]
  type FixedBack11[A, B, C, D, E, F, G, H, I, J, K] = Arity12[A, B, C, D, E, F, G, H, I, J, K, Unit]

  type Renamed12[A, B, C, D, E, F, G, H, I, J, K, L] = Arity12[A, B, C, D, E, F, G, H, I, J, K, L]
  type Extra12[A, B, C, D, E, F, G, H, I, J, K, L] = Arity11[A, B, C, D, E, F, G, H, I, J, K]
  type FixedFront12[A, B, C, D, E, F, G, H, I, J, K, L] = Arity13[Unit, A, B, C, D, E, F, G, H, I, J, K, L]
  type FixedBack12[A, B, C, D, E, F, G, H, I, J, K, L] = Arity13[A, B, C, D, E, F, G, H, I, J, K, L, Unit]

  type Renamed13[A, B, C, D, E, F, G, H, I, J, K, L, M] = Arity13[A, B, C, D, E, F, G, H, I, J, K, L, M]
  type Extra13[A, B, C, D, E, F, G, H, I, J, K, L, M] = Arity12[A, B, C, D, E, F, G, H, I, J, K, L]
  type FixedFront13[A, B, C, D, E, F, G, H, I, J, K, L, M] = Arity14[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M]
  type FixedBack13[A, B, C, D, E, F, G, H, I, J, K, L, M] = Arity14[A, B, C, D, E, F, G, H, I, J, K, L, M, Unit]

  type Renamed14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = Arity14[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
  type Extra14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = Arity13[A, B, C, D, E, F, G, H, I, J, K, L, M]
  type FixedFront14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = Arity15[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N]
  type FixedBack14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = Arity15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Unit]

  type Renamed15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = Arity15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
  type Extra15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = Arity14[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
  type FixedFront15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = Arity16[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
  type FixedBack15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = Arity16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Unit]

  type Renamed16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = Arity16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
  type Extra16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = Arity15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
  type FixedFront16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = Arity17[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
  type FixedBack16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = Arity17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Unit]

  type Renamed17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = Arity17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
  type Extra17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = Arity16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
  type FixedFront17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = Arity18[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
  type FixedBack17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = Arity18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Unit]

  type Renamed18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = Arity18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
  type Extra18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = Arity17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
  type FixedFront18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = Arity19[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
  type FixedBack18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = Arity19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Unit]

  type Renamed19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = Arity19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
  type Extra19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = Arity18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
  type FixedFront19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = Arity20[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
  type FixedBack19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = Arity20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Unit]

  type Renamed20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = Arity20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
  type Extra20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = Arity19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
  type FixedFront20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = Arity21[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
  type FixedBack20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = Arity21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Unit]

  type Renamed21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = Arity21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
  type Extra21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = Arity20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
  type FixedFront21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = Arity22[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
  type FixedBack21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = Arity22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Unit]

  type Renamed22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = Arity22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
  type Extra22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = Arity21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
  type FixedFront22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = Arity23[Unit, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
  type FixedBack22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = Arity23[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Unit]
}
// format: on
