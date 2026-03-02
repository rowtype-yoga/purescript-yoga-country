// Generates src/Data/Country.purs from ISO 3166-1 alpha-2 codes
import { readFileSync } from "fs"

const countries = JSON.parse(readFileSync("/tmp/countries.json", "utf8"))
  .sort((a, b) => a.alpha2.localeCompare(b.alpha2))

// "Côte d'Ivoire" -> "CoteDIvoire"
// "Korea, Republic of" -> "KoreaRepublicOf"
const toConstructor = (name) =>
  name
    .normalize("NFD").replace(/[\u0300-\u036f]/g, "") // strip accents
    .replace(/[^a-zA-Z0-9 ]/g, " ")                   // non-alphanum -> space
    .split(/\s+/)
    .filter(Boolean)
    .map(w => w.charAt(0).toUpperCase() + w.slice(1).toLowerCase())
    .join("")

// Deduplicate constructors (shouldn't happen but safety)
const seen = new Set()
const entries = countries.map(c => {
  let ctor = toConstructor(c.name)
  if (seen.has(ctor)) ctor = ctor + "_" + c.alpha2
  seen.add(ctor)
  return { ...c, ctor }
})

const n = entries.length
const lines = []

lines.push(`module Data.Country where`)
lines.push(``)
lines.push(`import Prelude`)
lines.push(``)
lines.push(`import Control.Monad.Except (throwError)`)
lines.push(`import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), enumFromTo)`)
lines.push(`import Data.Maybe (Maybe(..))`)
lines.push(`import Foreign (ForeignError(..))`)
lines.push(`import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)`)
lines.push(``)

// ADT
lines.push(`data Country`)
entries.forEach((e, i) => {
  const prefix = i === 0 ? "  = " : "  | "
  lines.push(`${prefix}${e.ctor}`)
})
lines.push(``)
lines.push(`derive instance Eq Country`)
lines.push(`derive instance Ord Country`)
lines.push(``)
lines.push(`instance Show Country where`)
lines.push(`  show = toAlpha2`)
lines.push(``)

// Bounded
lines.push(`instance Bounded Country where`)
lines.push(`  bottom = ${entries[0].ctor}`)
lines.push(`  top = ${entries[n - 1].ctor}`)
lines.push(``)

// Enum
lines.push(`instance Enum Country where`)
lines.push(`  succ = case _ of`)
entries.forEach((e, i) => {
  if (i < n - 1) lines.push(`    ${e.ctor} -> Just ${entries[i + 1].ctor}`)
  else lines.push(`    ${e.ctor} -> Nothing`)
})
lines.push(`  pred = case _ of`)
entries.forEach((e, i) => {
  if (i === 0) lines.push(`    ${e.ctor} -> Nothing`)
  else lines.push(`    ${e.ctor} -> Just ${entries[i - 1].ctor}`)
})
lines.push(``)

// BoundedEnum
lines.push(`instance BoundedEnum Country where`)
lines.push(`  cardinality = Cardinality ${n}`)
lines.push(`  toEnum = case _ of`)
entries.forEach((e, i) => lines.push(`    ${i} -> Just ${e.ctor}`))
lines.push(`    _ -> Nothing`)
lines.push(`  fromEnum = case _ of`)
entries.forEach((e, i) => lines.push(`    ${e.ctor} -> ${i}`))
lines.push(``)

// toAlpha2
lines.push(`toAlpha2 :: Country -> String`)
lines.push(`toAlpha2 = case _ of`)
entries.forEach(e => lines.push(`  ${e.ctor} -> "${e.alpha2}"`))
lines.push(``)

// fromAlpha2
lines.push(`fromAlpha2 :: String -> Maybe Country`)
lines.push(`fromAlpha2 = case _ of`)
entries.forEach(e => lines.push(`  "${e.alpha2}" -> Just ${e.ctor}`))
lines.push(`  _ -> Nothing`)
lines.push(``)

// allCountries
lines.push(`allCountries :: Array Country`)
lines.push(`allCountries = enumFromTo bottom top`)
lines.push(``)

// ReadForeign / WriteForeign (serialise as alpha-2)
lines.push(`instance WriteForeign Country where`)
lines.push(`  writeImpl = toAlpha2 >>> writeImpl`)
lines.push(``)
lines.push(`instance ReadForeign Country where`)
lines.push(`  readImpl f = do`)
lines.push(`    let parse s = case fromAlpha2 s of`)
lines.push(`          Just c -> pure c`)
lines.push(`          Nothing -> throwError $ pure $ ForeignError $ "Unknown country: " <> s`)
lines.push(`    s <- readImpl f`)
lines.push(`    parse s`)
lines.push(``)

process.stdout.write(lines.join("\n") + "\n")
