\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The Girl I Left Behind Me"}}
  composer = \markup\oldStyleNum"Folk Song"
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
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 100))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  f'8[ e] |
  d4 bes a4. g8 |
  a4 f d e |
  f4. f8 f[ g] a[ bes] |
  
  c4.( bes8) a4 \bar"" f'8[ e] |
  d4 c8[ bes] a4. g8 |
  a4 f d f |
  e g c, d8[ e] |
  
  f2 f4 \bar"" c'8[ bes] |
  a4 c d e |
  f c a f8[ g] |
  \slurDashed a4( c) \slurSolid d e |
  
  f2 e4 \bar"" f8[ e] |
  d4 c8[ bes] a4 \tieDashed g8~ g |
  a4 f d\fermata e8 f |
  e4 g c, d8[ e] |
  f2 f4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The dames of France are fond and free,
  And Flem -- ish lips are will -- ing,
  And soft the maids of It -- a -- ly,
  And Span -- ish eyes are thrill -- ing;

  Still, though I bask be -- neath their smile,
  Their charms fail to bind me,
  \set ignoreMelismata = ##t And my \unset ignoreMelismata heart falls back to E -- rin’s Isle
  To the girl I left be -- hind me.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  For she’s as fair as Shan -- non’s side,
  And pur -- er than its wa -- ter,
  But she re -- fused to be my bride,
  Though man -- y~a year I sought her;

  Yet, since to France I sailed a -- way,
  Her \set ignoreMelismata = ##t let -- ters \unset ignoreMelismata oft re -- mind me,
  \set ignoreMelismata = ##t That I \unset ignoreMelismata prom -- ised nev -- er to gain -- say
  "" The girl I left be -- hind me.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  She says, “My own dear love, come home,
  My friends are rich and man -- y,
  Or else, a -- broad with you I’ll roam,
  A sol -- dier stout as an -- y;

  If you’ll not come nor let me go,
  I’ll \set ignoreMelismata = ##t think you \unset ignoreMelismata have re -- signed me.”
  My heart nigh broke \set ignoreMelismata = ##t when I \unset ignoreMelismata an -- swered, “No,”
  To the girl I left be -- hind me.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  For nev -- er shall my true love brave
  A life of war and toil -- ing,
  And nev -- er as a skulk -- ing slave
  I’ll tread my na -- tive soil on;

  But, were it free or to be freed,
  The \set ignoreMelismata = ##t bat -- tle’s \unset ignoreMelismata close would find me
  To Ire -- land bound, nor mes -- sage need
  From the girl I left be -- hind me.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  a'4 |
  f f f4. e8 |
  f4 c d c |
  c4. c8 c4 f 
  
  f2 f4 a4 
  f f f4. e8 
  f4 c d d 
  c8[ d] e4 c c |
  
  c2 c4 e |
  f f f g |
  a f f c |
  \tieDashed f~ f \tieSolid f g |
  
  a2 g4 a8[ c] |
  bes4 f f4 \tieDashed e8~ e |
  f4 c d c8 c |
  c8[ d] e4 c c |
  c2 c4 \bar"|."
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
  c4 |
  bes d c4. bes8 |
  c4 a bes bes |
  a4. a8 a4 c |
  
  a2 c4 c |
  bes a c4. bes8 |
  c4 a bes bes |
  g bes g bes |
  
  a2 a4 g |
  f a bes c |
  c a c a |
  \slurDashed c( a) \slurSolid bes c |
  
  c2 c4 c |
  bes4 d c4 \tieDashed bes8~ bes |
  c4 a bes g8 a |
  g4 bes g bes |
  a2 a4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 |
  bes4 bes f4. f8 |
  f4 f f g |
  f4. f8 f4 f
  
  f2 f4 f
  f f f4. f8
  f4 f f f 
  c c e c 
  
  f2 f4 c |
  f f f f |
  f f f f |
  \tieDashed f~ f \tieSolid f f |
  
  f2 c4 f |
  f f f4 \tieDashed f8~ f |
  f4 f f\fermata c8 c |
  c4 c e c |
  f2 f4 \bar"|."
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
    \tempo 4 = 180
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


