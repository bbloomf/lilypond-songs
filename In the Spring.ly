\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"In the Spring"}}
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
       (padding . 0)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.95\in
  outer-margin = 0.7\in
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f4 f f8([ g] a4) |
  g4\mf g g8([ a] bes4) |
  a4.\f bes8 c4 bes |
  
  a g f2 |
  a4.\p bes8 c4 d |
  c8[ bes] a[ g] bes4 a |
  a4.\cresc bes8 c4 d |
  
  c8[ bes] a[ g] bes4 a |
  a4\f a g g8[ c] |
  e4 d c2 |
  
  f,4\p f f8([ g] a4) |
  g4\cresc g g8([ a] bes4) |
  a4.\f bes8 c[ f] d[ bes] |
  a4 g f2 \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	In the Spring, in the Spring,
  Sweet and fresh is ev -- ’ry -- thing;
  
  Win -- ter winds no more are blow -- ing,
  Blos -- soms fair a -- gain are grow -- ing,
  Gai -- ly mounts the lark on high!
  
  In the Spring, in the Spring,
  Sweet and fresh is ev -- ’ry -- thing.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  As God will, as God will,
  My fond heart yearns toward Him still.
  
  Should the heav’ns be o -- ver -- cloud -- ed,
  All the earth in dark -- ness shroud -- ed,
  Light will sure -- ly shine a -- gain.
  
  As God will, as God will,
  My fond heart yearns toward Him still.
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Hush, my heart, hush, my heart!
  Joy will come and pain de -- part.
  
  If in sor -- row thou art weep -- ing,
  Great -- er peace thou shalt be reap -- ing,
  Ev -- er lift thine eyes a -- bove.
  
  Hush, my heart, hush, my heart!
  Joy will come and pain de -- part.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  f4\p f f2 |
  e4 e g2 |
  f4. f8 g4 g |
  
  f e f2 |
  f4. f8 g4 bes |
  g f8[ e] f4 f |
  f4. f8 g4 bes |
  
  g f8[ e] f4 f |
  f f f e8[ g] |
  g4 g8[ f] e2 |
  
  f4 f f2 |
  e4 f e2 |
  f4. f8 f4 f8[ g] |
  f4 e c2 \bar"|."
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
  a4 a a8([ bes] c4) |
  c4 c d2 |
  d4. d8 e4 d |
  
  c4 c8[ bes] a2 |
  c4. d8 e4 f |
  e c d c |
  d4. d8 e4 f |
  
  e4 c d c |
  c d d c |
  c b g8([ a] bes4) |
  
  a4 a a8([ bes] c4) |
  c d c2 |
  f4. e8 ees4 d4 |
  c c8[ bes] a2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 f f2 |
  c'4 c bes8([ a] g4) |
  d4. d8 c4 g |
  
  a8([ bes] c4) f2 |
  f4. d8 c4 bes |
  c c' f, f |
  d4. d8 c4 bes |
  
  c4 c' f, f |
  f d8[ c] b4 c8[ e] |
  g4 g c,2 |
  f4 f f2 |
  c'4 b c2 |
  f,4. g8 a4 bes |
  c c, f2 \bar"|."
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
    \tempo 4 = 150
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


