\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #68
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key d \major
  \time 3/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  a'8 a8. d16 |
  d16[ cis] cis4 |
  g8 g8. b16 |
  b[ a] a4 |
  fis8 b a |
  
  a16[ gis] g4 |
  g8 fis e |
  b' a4 |
  
  a8 a8. d16 |
  d16 cis cis4 |
  g8 g8. b16 |
  b[ a] a4 |
  fis8 b a |
  
  a16[ gis] g4 |
  g8 fis e |
  b' a4 |
  \break
  \bar "||"
  fis'8 e d |
  cis16 b e4 |
  
  e8 d b |
  gis16 a d4 |
  fis16[ d] d[ a] a[ fis] |
  g[ e'] e4 |
  e8 b8. cis16 |
  e8 d4 |\break
  
  
  
  fis8 e d |
  cis16 b e4 |
  e8 d b |
  
  gis16 a d4 |
  fis16[ d] d[ a] a[ fis] |
  g[ e'] e4 |
  e8 fis8.\fermata e16 |
  e8 d4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Now ’neath the \set ignoreMelismata = ##t  sil -- ver moon \unset ignoreMelismata
  O -- cean is glow -- ing,
  O’er the calm bil -- low
  Soft winds are blow -- ing.
  Here balm -- y zeph -- yrs blow,
  Pure joys in -- vite us,
  And as we \set ignoreMelismata = ##t gent -- ly row \unset ignoreMelismata
  All things de -- light us.
  
  
  Hark how the sail -- or’s cry
  Joy -- ous -- ly ech -- oes nigh:
  San -- ta Lu -- ci -- a!
  San -- ta Lu -- ci -- a!
  
  Home of fair Po -- e -- sy,
  Realm of pure Har -- mo -- ny,
  San -- ta Lu -- ci -- a!
  San -- ta Lu -- ci -- a!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  When o’er thy wa -- ters
  Light winds are play -- ing,
  Thy spell can soothe us,
  All care al -- lay -- ing.
  
  To thee, sweet Na -- po -- li,
  What charms are giv -- en,
  Where smile’s cre -- a -- tion,
  Toil blest by heav -- en.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  fis8 fis8. fis16 |
  g[ g] g4 |
  e8 e8. g16 |
  g[ fis] fis4 |
  d8 g fis |
  
  fis16[ eis] e4 |
  e8 d cis |
  g' fis4 |
  
  fis8 fis8. fis16 |
  g g g4 |
  e8 e8. g16 |
  g[ fis] fis4 |
  d8 g fis |
  
  fis16[ eis] e4 |
  e8 d cis |
  g' fis4 |
  
  d'8 cis b |
  a16 g g4 |
  
  cis8 b g |
  eis16 fis fis4 |
  a8 fis fis16[ d] |
  e8 g4 |
  g8 g8. g16 g8 fis4 |
  
  
  d'8 cis b |
  a16 g g4 |
  cis8 b g |
  
  eis16 fis fis4 |
  a8 fis fis16[ d] |
  e8 g4 |
  g8 a8. g16 |
  g8 fis4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a8 a8. a16 |
  a[ a] a4 |
  cis8 a cis |
  d d4 |
  a8 a a |
  
  a8 a4 |
  a8 a a |
  b8 d4 |
  
  a8 a8. a16 |
  a a a4 |
  cis8 a cis |
  d d4 |
  a8 a a |
  
  a8 a4 |
  a8 a a |
  b8 d4 |
  
  
  a8 g b |
  e16 b b4 |
  
  g8 g b |
  b16 d a4 |
  d8 a d |
  a cis4 |
  a8 e'8. cis16 |
  d8 a4 |
  
  
  a8 g b |
  e16 b b4 |
  g8 g b |
  
  b16 d a4 |
  d8 a d |
  a cis4 |
  cis8 cis8. cis16 |
  d8 a4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,8 d8. d16 |
  e[ e] e4 |
  a,8 cis a |
  d8 a'4 |
  d,8 d d |
  
  cis8 cis4 |
  cis8 d a |
  d a'4 |
  
  d,8 d8. d16 |
  e e e4 |
  a,8 cis a |
  d8 a'4 |
  d,8 d d |
  
  cis8 cis4 |
  cis8 d a |
  d a'4 |
  
  d,8 e g |
  g16 g e4 |
  
  g8 g g |
  d16 d d4 |
  d8 d d |
  a a'4 |
  a,8 a'8. a16 |
  d,8 d4 |
  
  
  d8 e g |
  g16 g e4 |
  g8 g g |
  
  d16 d d4 |
  d8 d d |
  a a'4 |
  a,8 a'8.\fermata a16 |
  d,8 d4 \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" \transpose d des { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" \transpose d des { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" \transpose d des { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" \transpose d des { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Santa Lucia"}}
  composer = \markup\oldStyleNum"Neapolitan Folk Song"
  tagline = ""
}}


