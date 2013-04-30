\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Jamie’s on the Stormy Sea"}}
  composer = \markup\oldStyleNum"Bernard Covert, 1847"
  tagline = ""
}
\paper {
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
  first-page-number = #84
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
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	ees8. ees16 ees8 ees |
  g ees g16 bes8. |
  c ees16 bes8 g |
  f8. ees16 c ees8. |
  
  ees8. ees16 ees8 ees |
  g ees g16 bes8. |
  c8 ees bes8. g16 |
  f8. ees16 ees4 |
  
  bes'8 c ees bes |
  c8. bes16 g bes8. |
  c bes16 bes8 g |
  f ees c16 ees8. |
  
  ees ees16 ees8 ees |
  g ees g16 bes8. |
  c16 ees8.\fermata \acciaccatura c16 bes8.\fermata g16 |
  f8. ees16 ees4\fermata \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Ere the twi -- light bat was flit -- ting,
  In the sun -- set, at her knit -- ting,
  Sang a lone -- ly maid -- en, sit -- ting
  Un -- der -- neath her thes -- hold tree;
  And, ere day -- light died be -- fore us,
  And the ves -- per stars shone o’er us,
  Fit -- ful rose her ten -- der cho -- rus,
  “Ja -- mie’s on the storm -- y sea!”
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Warm -- ly shone the sun -- set glow -- ing;
  Sweet -- ly breathed the young flow’rs blow -- ing;
  Earth with beau -- ty o -- ver -- flow -- ing,
  Seemed the home of love to be.
  As those an -- gel tones as -- cend -- ing,
  With the scene and sea -- son blend -- ing,
  Ev -- er had the same low end -- ing,
  “Ja -- mie’s on the storm -- y sea!”
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Cur -- few bells re -- mote -- ly ring -- ing
  Min -- gled with that sweet voice sing -- ing,
  And the last red ray seemed cling -- ing,
  Lin -- g’ring -- ly to tower and tree;
  Near -- er as I came, and near -- er,
  Fin -- er rose the notes and clear -- er!
  Oh! ’twas heav’n it -- self to hear her,
  “Ja -- mie’s on the storm -- y sea!”
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  “Blow ye west winds! bland -- ly hov -- er
  O’er the bark that bears my lov -- er;
  Gent -- ly blow, and bear him o -- ver
  To his own dear home and me;
  For, when night winds bend the wil -- low,
  Sleep for -- sakes my lone -- ly pil -- low,
  Think -- ing of the foam -- ing bil -- low,
  “Ja -- mie’s on the storm -- y sea!”
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  How could I but list, but lin -- ger,
  To the song, and near the sing -- er,
  Sweet -- ly woo -- ing heav’n to bring her
  Ja -- mie from the storm -- y sea;
  And while yet her lips did name me,
  Forth I sprang, my hear o’er -- came me;
  “Grieve no more, sweet, I am Ja -- mie,
  Home re -- turned to love and thee!”
}

altoMusic = \relative c' {
  bes8. bes16 bes8 bes |
  ees ees ees16 ees8. |
  ees ees16 ees8 ees |
  c8. c16 aes c8. |
  
  bes bes16 bes8 bes |
  ees ees ees16 ees8. |
  ees8 ees ees8. ees16 |
  d8. ees16 ees4 |
  
  ees8 ees ees ees |
  ees8. ees16 ees ees8. |
  ees ees16 ees8 ees |
  c c aes16 c8. |
  
  bes bes16 bes8 bes |
  ees ees ees16 ees8. |
  ees16 ees8. ees ees16 |
  d8. ees16 ees4 \bar"|."
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
  g8. g16 g8 g |
  bes g bes16 g8. |
  aes c16 bes8 bes |
  aes8. aes16 ees aes8. |
  
  g g16 g8 g |
  bes g bes16 g8. |
  aes8 c bes8. bes16 |
  aes8. g16 g4 |
  
  g8 aes c g |
  aes8. g16 bes g8. |
  aes8. g16 g8 bes |
  aes aes ees16 aes8. |
  
  g8. g16 g8 g |
  bes g bes16 g8. |
  aes16 c8. bes bes16 |
  aes8. g16 g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8. ees16 ees8 ees |
  ees ees ees16 ees8. |
  aes8. aes16 g8 ees |
  aes,8. aes16 aes aes8. |
  
  ees'8. ees16 ees8 ees |
  ees ees ees16 ees8. |
  aes8 aes g8. ees16 |
  bes8. ees16 ees4 |
  
  ees8 ees ees ees |
  ees8. ees16 ees ees8. |
  ees ees16 ees8 ees |
  aes, aes aes16 aes8. |
  
  ees'8. ees16 ees8 ees |
  ees ees ees16 ees8. |
  aes16 aes8.\fermata g\fermata ees16 |
  bes8. ees16 ees4\fermata \bar"|."
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
  \midi {
    \tempo 4 = 75
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
}


