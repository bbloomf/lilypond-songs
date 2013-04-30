\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Oft in the stilly night"}}
  composer = \markup\oldStyleNum"Scotch Air"
  poet = \markup\oldStyleNum"Thomas Moore (1779–1852)"
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
  first-page-number = #26
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
  \key aes \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDotted
}

sopMusic = \relative c' {
	c'4\p\< c8. bes16 |
  aes16\!\> f8. f8 aes\p |
  ees8. ees16\< aes8 c |
  bes[\! c16 des] c8\> b\rest |
  
  c4\p\< c8. bes16 |
  aes8.\!\> f16 f8 aes\p |
  ees16\< ees8. c'8.\! aes16 |
  bes4\> aes8\! b16\rest ees,\mf |
  
  ees8.\cresc aes16\! aes8. aes16 |
  bes8. aes16 aes8 b16\rest aes\f |
  c8. aes16 aes8 aes |
  bes4\> aes8\! b16\rest ees,\f |
  
  ees8.\< aes16 aes8. aes16 |
  bes8.\!\> aes16 aes8\! b16\rest aes
  c8.\< aes16 aes8 aes\!^\markup\italic"rit." |
  bes[\> c16 des] c8\fermata\! b\rest\fermata |
  
  c4\pp\< c8. bes16 |
  aes16\!\> f8. f8 aes\! |
  ees8. ees16\< aes8 c |
  bes[\!\> c16 des] c8\! b\rest |
  
  c4\pp\< c8. bes16 |
  aes8.\!\> f16 f8[ g16] aes |
  ees\pp\< ees8. c' aes16\!^\markup\italic"rit." |
  bes4\> aes8 b\rest\fermata\! \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1."
	Oft in the stil -- ly night,
  Ere slum -- ber’s chain hath bound me,
  Fond mem -- ’ry brings the light
  Of oth -- er days a -- round me.
  The smiles, the tears of boy -- hood’s years,
  The words of love then spo -- ken,
  The eyes that shone, now dimm’d and gone,
  The cheer -- ful hearts now bro -- ken!
  
  Thus, in the stil -- ly night,
  Ere slum -- ber’s chain hath bound me,
  Sad mem -- ’ry brings the light
  Of oth -- er days a -- round me.
}

sopWordsII = \lyricmode {
  \set stanza = #"2."
  When I re -- mem -- ber all
  The friends so link’d to -- geth -- er,
  I’ve seen a -- round me fall
  Like leaves in au -- tumn weath -- er,
  I feel like one who treads a -- lone
  Some ban -- quet hall de -- sert -- ed,
  Whose lights are fled, whose gar -- lands dead,
  And all but he de -- part -- ed.
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
  aes'4 ees8. e16 |
  f16 des8. des8 des |
  c8. c16 ees8 ees |
  ees4 ees8 s |
  
  aes4 ees8. e16 |
  f8. des16 des8 des |
  c16 ees8. aes8. ees16 |
  g4 ees8 s16 c |
  
  c8. f16 ees8. aes16 |
  g8. aes16 ees8 s16 ees |
  aes8. ees16 f8 f |
  aes[ g] aes s16 c, |
  
  c8. f16 ees8. aes16 |
  g8. aes16 ees8 s16 ees |
  aes8. ees16 f8 f |
  aes[ g] aes8 s |
  
  aes4 ees8. e16 |
  f des8. des8 des |
  c8. c16 ees8 ees |
  ees4 ees8 s |
  
  aes4 ees8. e16 |
  f8. des16 des8. f16 |
  c ees8. aes ees16 |
  ees8[ des] c s \bar"|."
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
  ees4 c8. c16 |
  des aes8. aes8 aes |
  aes8. aes16 aes8 aes |
  g8[ aes16 bes] aes8 s |
  
  ees'4 c8. c16 |
  des8. aes16 aes8 aes8 |
  aes16 c8. ees8. c16 |
  ees8[ des] c s16 aes |
  
  aes8. des16 c8. ees16 |
  ees8[ des16] c c8 s16 c |
  ees8. c16 c8 c |
  f[ ees] c s16 aes |
  
  aes8. des16 c8. ees16 |
  ees8[ des16] c c8 s16 c |
  ees8. c16 c8 c |
  f[ ees] ees s |

  ees4 c8. c16 |
  des aes8. aes8 aes |
  aes8. aes16 aes8 aes |
  g8[ aes16 bes] aes8 s |
  
  ees'4 c8. c16 |
  des8. aes16 aes8. aes16 |
  aes c8. ees c16 |
  g4 aes8 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes4 aes8. aes16 |
  des, des8. des8 f |
  aes8. ees16 c8 aes |
  ees'4 aes,8 d\rest |
  
  aes'4 aes8. aes16 |
  des,8. des16 des8 f |
  aes16 aes8. aes8. aes16 |
  ees4 aes8 d,16\rest aes |
  
  aes8. aes'16 aes8. aes16 |
  ees8. aes16 aes8 d,16\rest aes'16 |
  aes8. aes16 f8 f |
  des[ ees] aes, d16\rest aes |
  
  aes8. aes'16 aes8. aes16 |
  ees8. aes16 aes8 d,16\rest aes' |
  aes8. aes16 f8 f |
  des[ ees] aes,\fermata d\rest\fermata |
  
  aes'4 aes8. aes16 |
  des, des8. des8 f |
  aes8. ees16 c8 aes |
  ees'4 aes,8 d\rest |
  
  aes'4 aes8. aes16 |
  des,8. des16 des8. des16 |
  ees ees8. ees ees16 |
  ees4 aes,8 d\rest\fermata \bar"|."
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
    \tempo 4 = 50
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


