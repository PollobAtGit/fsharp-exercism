module SpaceAge

type Planet =
    | Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

let age (planet: Planet) (n: int64) : float =
    let v = float n

    match planet with
    | Earth -> v / (86400.00 * 365.25)
    | Mercury -> v / (31557600.00 * 0.2408467)
    | Venus -> v / (31557600.00 * 0.61519726)
    | Mars -> v / (31557600.00 * 1.8808158)
    | Jupiter -> v / (31557600.00 * 11.862615)
    | Saturn -> v / (31557600.00 * 29.447498)
    | Uranus -> v / (31557600.00 * 84.016846)
    | Neptune -> v / (31557600.00 * 164.79132)
