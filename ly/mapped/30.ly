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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #30
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
  \key ees \major
  \time 3/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 8
  bes'8 |
  bes[ ees] d |
  bes[ d] c |
  bes[ \acciaccatura d c] bes |
  g4 g8 |
  bes aes e |
  \slurDashed
  f( bes8.) aes16 |
  
  \slurSolid
  g[ f]( ees4)~ |
  ees8 \bar""\break bes' bes |
  \slurDashed bes( ees) d |
  bes( d) c |
  bes( \slurSolid \acciaccatura d c) bes |
  g4 g8 |
  \slurDashed bes( aes) e |
  
  f( bes8.) aes16 |
  \slurSolid g[ f]( ees4)~ |
  ees \bar""\break bes'16 bes |
  \slurDashed c8( f) ees |
  ces( f) ees |
  bes[ f'] ees |
  \tieDashed bes4 bes16~ bes |
  
  a8[ bes] c |
  d4 d8 |
  \tieSolid d4.~ |
  d8\fermata \bar""\break c8 bes |
  \slurSolid
  bes[ ees] d8 |
  bes[ d] c |
  bes[ \acciaccatura d c] bes |
  
  g4 g16 g |
  bes8[ aes] e |
  f4 aes8 |
  g aes bes |
  c d ees |
  ees[ \acciaccatura { f16[ ees]} d8] ees |
  f[ c] d |
  g, aes bes |
  c d ees |
  ees[ \acciaccatura { f16[ ees]} d8] ees |
  g16[ f c8] d |
  ees4 \bar"|."
}
sopWords = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"1. "
  I dreamt _ I dwelt _ in mar -- _ ble halls,
  With vas -- sals and serfs at my side, _ _ _
  And of all who as -- sem -- bled with -- in __ _ those walls
  That I was the hope and the pride. __ _ _ _
  I had rich -- es too great _ to count; _ could boast
  Of a high _ an -- ces -- tral name; __ _
  \unset ignoreMelismata
  But I al -- so dreamt, which pleased me most,
  That you loved me still the same, that you loved me you loved me still the same,
  That you loved me, you loved me still the same.
}

sopWordsII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"2. "
  I dreamt _ that suit -- _ ors sought _ my hand;
  That knights up -- on bend -- _ ed knee, _ _ _
  And with vows _ no maid -- _ en heart could with -- stand,
  They pledged _ their faith _ to me, __ _ _ _
  And I dreamt _ that one of that no -- _ ble host
  Came _ forth _ my hand to claim; __ _
  \unset ignoreMelismata
  But I al -- so dreamt, which charmed me most,
  That you loved me still the same, that you loved me, you loved me still the same,
  That you loved me, you loved me still the same.
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
  g'8 |
  \tieDashed g4 g8 |
  g4 g8 |
  g4 g8 |
  ees4 ees8 |
  d d cis |
  d8~ d8. d16 |
  
  \tieSolid ees4.~ |
  ees8 g g |
  \tieDashed g~ g g |
  g~ g g |
  g8~ g g8 |
  ees4 ees8 |
  d~ d cis |
  
  d~ d8. d16 |
  \tieSolid ees4.~ |
  ees4 ees16 ees |
  \tieDashed aes8~ aes aes |
  aes~ aes aes |
  g4 g8 |
  g4 g16~ g |
  
  fis8[ g] a |
  fis4 fis8 |
  g4.( |
  aes8) aes aes |
  g4 g8 |
  g4 g8 |
  g4 g8 |
  
  ees4 ees16 ees |
  d4 cis8 |
  d4 f8 |
  ees f g |
  aes aes aes |
  g4 g8 |
  
  aes4 aes8 g f g |
  aes aes aes |
  g4 g8 |
  aes4 aes8 |
  g4 \bar"|."
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
  bes8 |
  \tieDashed bes4 bes8 |
  bes4 bes8 |
  ees4 ees8 |
  bes4 bes8 |
  bes bes bes |
  bes~ bes8. bes16 |
  
  g4.~ |
  g8 ees' ees |
  bes~ bes bes |
  bes~ bes bes |
  ees~ ees ees |
  bes4 bes8 |
  bes~ bes bes |
  
  bes~ bes8. bes16 |
  \tieSolid g4.~ |
  g4  g16 g |
  \tieDashed c8~ c c |
  ces~ ces ces |
  bes4 bes8 |
  ees4 c16~ c |
  
  d4 d8 |
  aes4 aes8 |
  b4.( |
  bes?8) bes bes |
  bes4 bes8 |
  bes4 bes8 |
  ees4 ees8 |
  
  bes4 bes16 bes |
  bes4 bes8 |
  bes4 bes8 |
  bes bes ees |
  ees d c |
  bes4 bes8 |
  
  bes4 bes8 |
  bes bes ees |
  ees d c |
  bes4 bes8 |
  d[ bes] bes |
  bes4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees4 ees8 |
  ees4 ees8 |
  ees4 ees8 |
  ees4 ees8 |
  bes bes bes |
  \tieDashed bes~ bes8. bes16 |
  
  \tieSolid ees4.~ |
  ees8 ees ees |
  \tieDashed ees~ ees ees |
  ees~ ees ees |
  ees~ ees ees |
  ees4 ees8 |
  bes~ bes bes |
  
  bes~ bes8. bes16 |
  \tieSolid ees4.~ |
  ees4 ees16 ees |
  \tieDashed ees8~ ees ees |
  ees~ ees ees |
  ees4 ees8 |
  ees4 ees16~ ees |
  
  d4 d8 |
  d4 d8 |
  g4.( |
  f8)\fermata d d |
  ees4 ees8 |
  ees4 ees8 |
  ees4 ees8 |
  
  ees4 ees16 ees |
  bes4 bes8 |
  bes4 d8 |
  ees ees ees |
  aes aes aes |
  bes4 bes,8 |
  
  bes4 bes8 |
  ees ees ees |
  aes aes aes |
  bes4 bes8 |
  bes4 f8 |
  ees4 \bar"|."
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
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"I dreamt I dwelt in marble halls"}}
  composer = \markup\oldStyleNum"Michael William Balfe (1808–1870)"
  tagline = ""
}}



