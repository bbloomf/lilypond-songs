\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Minstrel Boy"}}
  poet = \markup\oldStyleNum"Thomas Moore (1779–1852)"
  composer = \markup\oldStyleNum{"Irish Air," \italic"The Moreen"}
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
  first-page-number = #27
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  bes4\mf |
  ees4. f8 aes[ g] f ees |
  g4 bes ees d8. ees16 |
  c4. bes8 g8.[ aes16 bes8] g |
  f4..( ees16) ees4 bes |
  ees4. f8 aes[ g] f ees |
  
  g4 bes ees d8. ees16 |
  c4 bes g8.[ aes16 bes8] g |
  f2 ees4 << bes4\rest\f { s8 \teeny bes'8 \normalsize} >> |
  \slurDashed
  ees4-> d-> c-> bes8\rest d16( ees) |
  d4-> c-> bes-> bes8\rest bes |
  
  c4. g8 g4 g |
  \slurSolid
  c4.( d8) ees4 ees\fermata |
  \slurDashed
  ees,4. f8 aes[ g] f( ees) |
  g4 bes ees\fermata d8( ees) |
  c4. bes8 g8.[ aes16 bes8] g |
  f2 ees4 \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The min -- strel boy to the war is gone,
  In the ranks of death you’ll find him;
  His fa -- ther’s sword he hath gird -- ed on,
  And his wild harp slung be -- hind him.
  "" “Land of song!” \set ignoreMelismata = ##t said the war -- rior bard,
  \unset ignoreMelismata
  “Tho’ all the world be -- trays __ thee,
  One sword at least thy __ rights shall guard,
  One __ faith -- ful harp shall praise thee.”
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The min -- strel fell, but the foe -- man’s chain
  Could not bring his proud soul un -- der;
  The harp he lov’d nev -- er spoke a -- gain,
  For he tore its chords a -- sun -- der,
  And said, “No chains shall sul -- ly thee,
  Thou soul of love and brave -- ry!
  \set ignoreMelismata = ##t
  Thy songs were made _ for the pure and free,
  They shall nev -- er sound _ _ in slave -- ry.”
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
  \partial 4
  bes4 |
  ees4. ees8 ees4 bes8 bes |
  ees4 f ees ees8. ees16 |
  ees4. ees8 ees4. ees8 |
  d2 ees4 bes |
  ees4. ees8 ees4 bes8 bes |
  
  ees4 f ees ees8. ees16 |
  ees4 ees ees4. ees8 |
  d2 ees4 s8 \teeny g \normalsize |
  g4 f ees s8 \slurDashed f16( ees) |
  f4 ees d s8 f |
  
  ees4. ees8 d4 d |
  \slurSolid
  c4.( f8) ees4 ees |
  \slurDashed
  ees4. bes8 bes4 d8( ees) |
  ees4 f ees ees8( ees) |
  ees4. ees8 ees4. ees8 |
  d2 ees4 \bar"|."
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
  \partial 4
  g4 |
  g4. aes8 c[ bes] aes g |
  bes4 bes g bes8. bes16 |
  aes4. bes8 bes4. bes8 |
  bes4( aes) g g |
  g4. aes8 c[ bes] aes g |
  
  bes4 bes g bes8. bes16 |
  aes4 bes bes4. bes8 |
  \slurSolid
  bes4.( aes8) g4 s8 \teeny bes \normalsize |
  bes4 b c s8 \slurDashed b16( c) |
  bes?4 a bes s8 d |
  
  c4. c8 b4 g |
  \slurSolid g4.( b8) c4 c |
  \slurDashed bes4. aes8 f[ g] aes( g) |
  bes4 bes g bes8( bes) |
  aes4. bes8 bes4. bes8 |
  \slurSolid
  bes4.( aes8) g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  ees,4 |
  ees4. ees8 ees4 ees8 ees |
  ees4 d c g8. g16 |
  aes4. g8 ees'8.[ f16 g8] ees |
  bes2 <\tweak #'font-size #-3 ees ees,>4 ees |
  ees4. ees8 ees4 ees8 ees |
  
  ees4 d c g8. g16 |
  aes4 g ees'8.[ f16 g8] ees |
  bes2 ees4 << d4\rest {s8 \teeny ees8} >> \normalsize |
  \slurDashed
  ees4 g c, d8\rest g16( c,) |
  f4 < \tweak #'font-size #-3 f f,> bes, d8\rest f |
  
  c4. ees8 g4 f |
  \slurSolid ees4.( d8) c4 g'\fermata |
  \slurDashed g4. f8 d[ ees] bes( ees) |
  ees4 d c\fermata g8( g) |
  aes4. g8 ees'8.[ f16 g8] ees |
  bes2 ees4 \bar"|."
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
    \tempo 4 = 80
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


