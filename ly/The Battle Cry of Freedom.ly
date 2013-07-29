\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Battle Cry of Freedom"}}
  composer = \markup\oldStyleNum"George Frederick Root (1825–1895)"
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
       (padding . 1)
       (stretchability . 100))
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4
  aes'8 bes |
  c c c8. bes16 aes4 f8. g16 |
  aes8 aes aes8. g16 f2 |
  ees4 ees8. des16 c8 ees aes8. bes16 |
  c2 bes4 \bar""
  
  aes8 bes |
  c c c8. bes16 aes4 f8. g16 |
  aes8 aes aes8. g16 f2 |
  ees4 ees8. des16 c8 ees aes8. c16 |
  bes2 aes4 \bar"||"\break
  
  b8\rest ees |
  ees4 c8. des16 ees8 f4 ees8 |
  ees4 c8. des16 ees2 |
  ees4 c8. des16 ees8 f4. |
  ees4 c8. aes16 bes4 \bar"" aes8 bes 
  
  c8 c c8. bes16 aes4 f |
  aes8 aes aes8. g16 f2 |
  ees4 ees8. des16 c8 ees aes8. c16 |
  bes2 aes4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Yes, we’ll ral -- ly round the flag, boys, we’ll ral -- ly once a -- gain,
  Shout -- ing the bat -- tle -- cry of Free -- dom,
  We will ral -- ly from the hill -- side, we’ll gath -- er from the plain,
  Shout -- ing the bat -- tle -- cry of Free -- dom!
  
  The U -- nion for -- ev -- er, Hur -- rah, boys, Hur -- rah!
  Down with the trai -- tor, Up with the stars;
  While we ral -- ly round the flag, boys, ral -- ly once a -- gain,
  Shout -- ing the bat -- tle -- cry of Free -- dom.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  We are spring -- ing to the call of our bro -- thers gone be -- fore,
  Shout -- ing the bat -- tle cry of free -- dom!
  And we’ll fill our va -- cant ranks with a mil -- lion free -- men more,
  Shout -- ing the bat -- tle cry of free -- dom!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  We will wel -- come to our num -- bers the loy -- al, true, and brave,
  Shout -- ing the bat -- tle cry of free -- dom!
  And al -- though they may be poor, not a man shall be a slave,
  Shout -- ing the bat -- tle cry of free -- dom!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  So we’re spring -- ing to the call from the East and from the West,
  Shout -- ing the bat -- tle cry of free -- dom;
  And we’ll hurl the reb -- el crew from the land that we love best,
  Shout -- ing the bat -- tle cry of free -- dom!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees8 des |
  c c c8. c16 c4 c8. c16 |
  c8 c c8. bes16 aes2 |
  c4 c8. bes16 aes8 c ees8. ees16 |
  ees2 ees4
  
  ees8 des |
  c c c8. c16 c4 c8. c16 |
  c8 c c8. bes16 aes2 |
  c4 c8. bes16 aes8 c ees8. ees16 |
  des2 c4
  
  
  s8 aes' |
  aes4 aes8. aes16 aes8 aes4 aes8 |
  aes4 aes8. aes16 aes2 |
  aes4 aes8. aes16 aes8 aes4. |
  ees4 f8. f16 g4 ees8 des |
  
  c c c8. c16 c4 c |
  c8 c c8. bes16 aes2 |
  c4 c8. bes16 aes8 c ees8. ees16 |
  des2 c4
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
  aes8 g |
  aes aes aes8. aes16 aes4 aes8. g16 |
  f8 f e8. e16 f2 |
  aes4 aes8. aes16 aes8 aes aes8. g16 |
  aes2 g4 
  
  aes8 g |
  aes aes aes8. aes16 aes4 aes8. g16 |
  f8 f e8. e16 f2 |
  aes4 aes8. aes16 aes8 aes aes8. aes16 |
  g2 aes4 
  
  
  s8 c8 |
  c4 aes8. bes16 c8 des4 c8 |
  c4 aes8. bes16 c2 |
  c4 aes8. bes16 c8 des4. |
  c4 aes8. aes16 g4 aes8 g |
  
  aes aes aes8. aes16 aes4 aes |
  f8 f e8. e16 f2 |
  aes4 aes8. aes16 aes8 aes aes8. aes16 |
  g2 aes4 
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,8 bes |
  aes aes aes8. aes16 f4 f8. f16 |
  c'8 c c8. c16 des2 |
  aes4 aes8. aes16 aes8 aes c8. ees16 |
  aes,2 ees'4 
  
  c8 bes |
  aes aes aes8. aes16 f4 f8. f16 |
  c'8 c c8. c16 des2 |
  aes4 aes8. aes16 aes8 aes c8. c16 |
  ees2 aes,4 
  
  
  d8\rest aes' |
  aes4 aes8. aes16 aes8 aes4 aes8 |
  aes4 aes8. aes16 aes2 |
  aes4 aes8. aes16 aes8 aes4. |
  aes4 f8. f16 ees8[ des] c8 bes |
  
  aes aes aes8. aes16 f4 f |
  c'8 c c8. c16 des2 |
  aes4 aes8. aes16 aes8 aes c8. c16 |
  ees2 aes,4 
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
    \tempo 4 = 100
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
