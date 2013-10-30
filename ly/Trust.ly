\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Trust"}}
  composer = \markup\oldStyleNum"Johann Rudolf Zumsteeg (1760–1802)"
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
  first-page-number = #17
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
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c'' {
	a4\p\< a4. a8 |
  a4\> g\! b\rest |
  c8[\< bes] a4. g8 |
  bes4\> a\! b\rest |
  d8[\p\< bes] f4. d'8 |
  
  c8.[\> d16] c4\! b\rest |
  bes8[\< g] d4. bes'8 |
  a8.[\> bes16] a4\! b\rest |
  g\mf g g\cresc |
  f'2. |
  e4 c b\> |
  c b\rest\! b\rest |
  
  c\p c bes |
  a2\< e'4 |
  f8.[\! c16] c4 bes | \break
  a b\rest b\rest |
  
  %page2
  c4\p c8[ bes] a[ g] |
  f4 f b\rest |
  d d8[ c] bes[ a] |
  g8.[(\< a32 g] fis8[ g a bes] |
  c4)\! c8.[\mf\> d16] c4 |
  
  b4\rest\! c8.[\> d16] c4 |
  d2.~\f |
  d4 a g\p |
  bes2.~\< |
  bes4 b\rest\! b\rest |
  d4\p c\dim c |
  a2.~\pp |
  a4 b\rest b\rest \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Cloud -- rifts must van -- ish,
  cloud -- rifts must van -- ish,
  Griev -- ing to ban -- ish,
  Look to the mor -- row,
  Search -- ing with -- in,
  search -- ing with -- in.
  
  End -- ed is sor -- row,
  Joy may be -- gin!
  End -- ed is sor -- row,
  Joy may be -- gin! __
  
  Joy may,
  joy may,
  joy __ may be -- gin! __
  Joy may be -- gin! __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Star -- light ef -- ful -- gent,
  star -- light ef -- ful -- gent,
  Sheds its in -- dul -- gent
  Ra -- di -- ance, shed -- ding
  Heav -- en -- ly rest,
  heav -- en -- ly rest.
  
  Earth -- ward ’tis spread -- ing,
  Peace in my breast,
  Earth -- ward ’tis spread -- ing,
  Peace in my breast, __
  
  Peace, _
  peace, _
  peace __ in my breast, __
  Peace in my breast. __
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Ev -- er -- more dar -- ing,
  ev -- er -- more dar -- ing,
  Nev -- er de -- spair -- ing,
  Brave I then ev -- er
  Fate’s dir -- est ways,
  fate’s dir -- est ways.
  
  Faint -- heart -- ed nev -- er,
  Up -- ward my gaze!
  Faint -- heart -- ed nev -- er,
  Up -- ward my gaze! __
  
  Up -- ward,
  up -- ward,
  up -- ward my gaze! __
  Up -- ward my gaze! __
}

altoMusic = \relative c' {
  f f4. f8 |
  f4 e s |
  a8[ g] f4. e8 |
  g4 f s |
  f f4. f8 |
  
  g4 a s |
  d, d4. g8 |
  e4 f s |
  d d c |
  g'2. |
  g4 g f |
  e s s |
  
  a a g |
  f2 g4 |
  a a g |
  f s s |
  
  %page2
  a4 a8[ g] f[ e] |
  d4 d s |
  bes'4 bes8[ a] g[ f] |
  e8.([ f32 e] dis8[ e f g] |
  a4) g a | \break
  
  s g a |
  f2.~ |
  f4 f e |
  f2.~ |
  f4 s s |
  f f e |
  f2.~ |
  f4 s s \bar "|."
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
  c4 c4. c8 |
  c4 c s |
  c c4. c8 |
  c4 c s |
  bes8[ d] d4. d8 |
  
  e4 f s |
  g8[ d] bes4. d8 |
  cis4 d s |
  b4 b c? |
  b2. |
  c4 e d |
  c s s |
  
  s2. |
  c4\p c bes |
  a2\< e'4 |
  f8.[\! c16] c4 bes |
  
  %page2
  a4 s s |
  \once\override DynamicText #'extra-offset = #'( 0 . -4)
  \once\override DynamicText #'self-alignment-X = #4
  f'4\p f8[ e] d[ c] |
  bes4 bes s4 |
  c c4. c8 |
  c4 e f |
  
  s e f |
  d2.~ |
  d4 c c |
  bes2.~ |
  bes4 s s |
  bes4 a g8[ c] |
  c2.~ |
  c4 s s \bar "|."
}

tenorWords = \lyricmode {
  \repeat unfold 28 { \skip 1 }
  
  End -- ed is sor -- row,
  Joy may be -- gin!
  End -- ed is sor -- row,
  Joy may be -- gin!
}

tenorWordsII = \lyricmode {
  \repeat unfold 28 { \skip 1 }
  
  Earth -- ward ’tis spread -- ing,
  Peace in my breast,
  Earth -- ward ’tis spread -- ing,
  Peace in my breast,
}

tenorWordsIII = \lyricmode {
  \repeat unfold 28 { \skip 1 }
  
  Faint -- heart -- ed nev -- er,
  Up -- ward my gaze!
  Faint -- heart -- ed nev -- er,
  Up -- ward my gaze!
}

bassMusic = \relative c' {
  f,4 f4. f8 |
  c'4 c, d\rest |
  c c4. c8 |
  e4 f d\rest |
  bes'4 bes4. bes8 |
  
  bes4 a d,\rest |
  g g4. g8 |
  g4 f d\rest |
  f f e |
  d2. |
  c4 g' g |
  c,4 d\rest d\rest |
  
  d2.\rest |
  a'4 a g |
  f2 g4 |
  a a g |
  
  %page2
  f4 d\rest d\rest |
  d'4 d8[ c] bes[ a] |
  g4 g d\rest |
  c'4 c8[ bes] a[ g] |
  f4 bes a |
  
  d,\rest bes' a |
  bes8([ f d' bes f d] |
  bes4) c c |
  d2.~ |
  d4 d\rest d\rest |
  bes4 c c |
  f2.~ |
  f4 d\rest d\rest \bar "|."
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
    \new Lyrics = "altos"  \lyricsto "altos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "altos" \sopWordsIII
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 110
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


