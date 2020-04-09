\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Hard Times"}}
  composer = \markup\oldStyleNum"Stephen Foster (1826–1864)"
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
  first-page-number = #90
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
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  
}

sopMusic = \relative c' {
	\partial 4
  ees8 f |
  g4 g8 g g bes4 g8 |
  f ees ees f g4 c8. bes16 |
  bes4 g8~ g g ees f8. f16 |
  
  ees2 b'4\rest ees,8 f |
  g4 g8 g g bes4 g8 |
  f ees ees f g4 c\fermata |
  bes g g8 ees8 f8. f16 |
  ees2 b'4\rest \bar"||"\break
  
  g8 aes |
  bes4 bes8\rest bes bes4 a8 bes |
  c2 bes4 bes\rest |
  ees bes c bes |
  g8 g f ees f4\fermata ees8 f |
  
  g4 g8 g g bes4 g8 |
  f ees ees f g4 c\fermata |
  bes g g8 ees f8. f16 |
  ees2 b'4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Let us pause in life’s plea -- sures and count its ma -- ny tears
  While we all sup __ sor -- row with the poor;
  There’s a song that will lin -- ger for -- ev -- er in our ears;
  Oh! Hard Times, come a -- gain no more.
  
  ’Tis the song, the sigh of the wea -- ry;
  Hard Times, Hard Times, come a -- gain no more:
  Ma -- ny days you have lin -- gered a -- round my cab -- in door,
  Oh! Hard Times, come a -- gain no more.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  While we seek mirth and beau -- ty and mu -- sic light and gay
  There are frail forms __ faint -- ing at the door:
  Though their voic -- es are si -- lent, their plead -- ing looks will say:
  Oh! Hard Times, come a -- gain no more.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  There’s a pale droop -- ing maid -- en who toils her life a -- way,
  With a worn heart whose bet -- ter days are o’er;
  Though her voice would be mer -- ry, ’tis sigh -- ing all the day:
  Oh! Hard Times, come a -- gain no more.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  ’Tis a sigh that is waft -- ed a -- cross the trou -- bled wave,
  ’Tis a wail that is heard up -- on the shore,
  ’Tis a dirge that is mur -- mured a -- round the low -- ly grave:
  Oh! Hard Times, come a -- gain no more.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees8 d |
  ees4 ees8 ees ees ees4 ees8 |
  bes bes bes d ees4 |
  ees8. ees16 |
  ees4 ees8~ ees ees ees d8. d16 |
  
  ees2 s4 ees8 d |
  ees4 ees8 ees ees ees4 ees8 |
  bes bes bes d ees4 ees |
  ees ees ees8 bes bes8. bes16 |
  bes2 s4 |
  
  ees8 f |
  g4 s8 g g4 fis8 g |
  aes2 g4 s |
  g g aes g |
  ees8 ees c c d4 ees8 d |
  
  ees4 ees8 ees ees ees4 ees8 |
  bes bes bes d ees4 ees |
  ees ees ees8 bes d8. d16 |
  ees2 s4 \bar"|."
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
  g8 aes |
  bes4 bes8 bes bes g4 bes8 |
  aes g g bes bes4 aes8. g16 |
  g4 bes8~ bes bes g aes8. aes16 |
  
  g2 s4 g8 aes |
  bes4 bes8 bes bes g4 bes8 |
  aes g g bes bes4 aes |
  g bes bes8 g aes8. aes16 |
  g2 s4 |
  
  bes8 bes |
  ees4 s8 ees ees4 ees8 ees |
  ees2 ees4 s |
  bes ees ees ees |
  bes8 bes a a bes4 g8 aes |
  
  bes4 bes8 bes bes g4 bes8 |
  aes8 g g bes bes4 aes |
  g bes bes8 g aes8. aes16 |
  g2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 ees |
  ees4 ees8 ees ees ees4 ees8 |
  bes bes bes bes ees4 ees8. ees16 |
  bes4 bes8~ bes bes bes bes8. bes16 |
  
  ees2 d4\rest ees8 ees |
  ees4 ees8 ees ees ees4 ees8 |
  bes bes bes bes ees4 aes,\fermata |
  bes bes bes8 bes bes8. bes16 |
  ees2 d4\rest |
  
  ees8 ees |
  ees4 d8\rest ees ees4 ees8 ees |
  aes2 ees4 d\rest |
  ees ees ees ees |
  ees8 ees f f bes,4\fermata ees8 ees |
  
  ees4 ees8 ees ees ees4 ees8 |
  bes bes bes bes ees4 aes,\fermata |
  bes bes bes8 bes bes8. bes16 |
  ees2 d4\rest \bar"|."
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
    \tempo 4 = 85
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


